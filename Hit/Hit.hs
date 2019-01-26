{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module      : Hit
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
module Main where

import System.Environment
import Control.Applicative ((<$>))
import Control.Monad
import Data.IORef
import Data.Maybe
import Data.Git.Storage.Pack
import Data.Git.Storage.Object
import Data.Git.Storage
import Data.Git.Types
import Data.Git.Ref
import Data.Git.Repository
import Data.Git.Revision
import Data.Git.Diff
import Data.Hourglass
import Data.Word
import qualified Data.Set as S
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Char8 as BC
import Text.Printf
import qualified Data.Map as M
import qualified Data.HashTable.IO as H
import qualified Data.Hashable as Hashable

import Data.Git.Diff.Patience as AP (Item(..))

type HashTable k v = H.CuckooHashTable k v

instance Hashable.Hashable (Ref SHA1) where
    hashWithSalt salt = Hashable.hashWithSalt salt . toBinary

verifyPack pref git = do
    offsets     <- H.new
    tree        <- H.new
    refs        <- newIORef M.empty
    entries     <- fromIntegral <$> packReadHeader (gitRepoPath git) pref
    leftParsed  <- newIORef entries
    -- enumerate all objects either directly in tree for fully formed objects
    -- or a list of delta to resolves
    packEnumerateObjects (gitRepoPath git) pref entries (setObj leftParsed refs offsets tree)
    readIORefAndReplace refs M.empty >>= dumpTree offsets tree
  where
        readIORefAndReplace ioref emptyVal = do
            v <- readIORef ioref
            writeIORef ioref emptyVal
            return v

        setObj_ refs offsets tree (!info, objData)
            | objectTypeIsDelta (poiType info) = do
                (!ty, !ref, !ptr, !lenChain) <- do
                    let loc = Packed pref (poiOffset info)
                    objInfo <- maybe (error "cannot find delta chain") id <$> getObjectRawAt git loc True
                    let (ty, sz, _) = oiHeader objInfo
                    let !ref = objectHash ty sz (oiData objInfo)
                    let ptr = head $ oiChains objInfo -- it's safe since deltas always have a non empty valid chain
                    return (ty, ref, ptr, (length $ oiChains objInfo))
                H.insert tree ref (info { poiType = ty }, Just (ptr, lenChain))
            | otherwise = do
                let !ref = objectHash (poiType info) (poiActualSize info) objData
                modifyIORef refs (M.insert ref ())
                H.insert offsets (poiOffset info) ref
                H.insert tree ref (info,Nothing)

        setObj leftParsed refs offsets tree x = do
            parsed <- readIORef leftParsed
            when ((parsed `mod` 256) == 0) $ putStrLn (show parsed ++ " left to parse")
            modifyIORef leftParsed (\i -> i-1)
            setObj_ refs offsets tree x

        dumpTree :: HashTable Word64 (Ref SHA1) -> HashTable (Ref SHA1) (PackedObjectInfo SHA1, Maybe (ObjectPtr SHA1, Int)) -> M.Map (Ref SHA1) () -> IO ()
        dumpTree offsets tree refs = do
            forM_ (M.toAscList refs) $ \(ref, ()) -> do
                ent <- fromJust <$> H.lookup tree ref
                printEnt offsets ref ent

        -- print one line about the entry
        -- format is <sha1> <type> <real size> <size> <offset> [<number of chain element> <parent element>]
        printEnt _ ref (info,Nothing) = do
            printf "%s %-6s %d %d %d\n" (show ref)
                   (objectTypeMarshall $ poiType info)
                   (poiActualSize info)
                   (poiSize info)
                   (poiOffset info)

        printEnt offsets ref (info,Just (parentOffset, lenChain)) = do
            parentRef <- case parentOffset of
                PtrRef r -> return r
                PtrOfs off -> do
                    let poff = poiOffset info - off
                    maybe (error "cannot find delta's parent in pack ?") id <$> H.lookup offsets poff
            printf "%s %-6s %d %d %d %d %s\n" (show ref)
                   (objectTypeMarshall $ poiType info)
                   (poiActualSize info)
                   (poiSize info)
                   (poiOffset info)
                   (lenChain)
                   (show parentRef)


catFile ty ref git = do
    let expectedType = case ty of
                        "commit" -> Just TypeCommit
                        "blob"   -> Just TypeBlob
                        "tag"    -> Just TypeTag
                        "tree"   -> Just TypeTree
                        "-t"     -> Nothing
                        _        -> error "unknown type request"
    mobj <- getObjectRaw git ref True
    case mobj of
        Nothing  -> error "not a valid object"
        Just obj ->
            let (objty, _, _) = oiHeader obj in
            case expectedType of
                Nothing  -> putStrLn $ objectTypeMarshall objty
                Just ety -> do
                    when (ety /= objty) $ error "not expected type"
                    LC.putStrLn (oiData obj)

lsTree revision _ git = do
    ref <- maybe (error "revision cannot be found") id <$> resolveRevision git revision
    tree <- resolveTreeish git ref
    case tree of
        Just t -> do
            htree <- buildHTree git t
            mapM_ (showTreeEnt) htree
        _      -> error "cannot build a tree from this reference"
    where
        showTreeEnt (ModePerm p,n,TreeDir r _) = printf "%06o tree %s    %s\n" p (show r) (show n)
        showTreeEnt (ModePerm p,n,TreeFile r)  = printf "%06o blob %s    %s\n" p (show r) (show n)

revList revision git = do
    ref <- maybe (error "revision cannot be found") id <$> resolveRevision git revision
    loopTillEmpty ref
    where loopTillEmpty ref = do
                commit <- getCommit git ref
                putStrLn $ show ref
                -- this behave like rev-list --first-parent.
                -- otherwise the parents need to be organized and printed
                -- in a reverse chronological fashion.
                case commitParents commit of
                    []    -> return ()
                    (p:_) -> loopTillEmpty p

getLog revision git = do
    ref    <- maybe (error "revision cannot be found") id <$> resolveRevision git revision
    commit <- getCommit git ref
    printCommit ref commit
  where printCommit ref commit = do
            mapM_ putStrLn
                [ ("commit: " ++ show ref)
                , ("author: " ++ BC.unpack (personName author) ++ " <" ++ BC.unpack (personEmail author) ++ ">")
                , ("date:   " ++ timePrint ISO8601_DateAndTime (personTime author) ++ " (" ++ timePrint ISO8601_DateAndTime (personTime author) ++ ")")
                , ""
                , BC.unpack $ commitMessage commit
                ]
            return ()
          where author = commitAuthor commit

showDiff :: Revision -> Revision -> Git SHA1 -> IO ()
showDiff rev1 rev2 git = do
    ref1 <- maybe (error "revision cannot be found") id <$> resolveRevision git rev1
    ref2 <- maybe (error "revision cannot be found") id <$> resolveRevision git rev2
    diffList <- getDiffWith (defaultDiff 5) ([]) ref1 ref2 git
    mapM_ showADiff diffList
    where
        showADiff :: GitDiff SHA1 -> IO ()
        showADiff hd = do
            printFileName $ hFileName hd
            printFileMode $ hFileMode hd
            printFileRef  $ hFileRef  hd
            printFileDiff $ hFileContent hd

        printFileName :: EntPath -> IO ()
        printFileName filename = putStrLn $ "Hit.Diff on file: " ++ (show filename)

        printFileMode :: GitFileMode -> IO ()
        printFileMode (NewMode (ModePerm m)) = printf "new file mode: %06o\n" m
        printFileMode (OldMode (ModePerm m)) = printf "old file mode: %06o\n" m
        printFileMode (UnModifiedMode (ModePerm m)) = printf "current file mode: %06o\n" m
        printFileMode (ModifiedMode (ModePerm o) (ModePerm n)) = printf "file mode: %06o -> %06o\n" o n

        printFileRef :: GitFileRef SHA1 -> IO ()
        printFileRef (NewRef r) = putStrLn $ "+++ new/" ++ (show r)
        printFileRef (OldRef r) = putStrLn $ "--- old/" ++ (show r)
        printFileRef (UnModifiedRef r) = putStrLn $ "=== cur/" ++ (show r)
        printFileRef (ModifiedRef o n) = do putStrLn $ "+++ new/" ++ (show n)
                                            putStrLn $ "--- old/" ++ (show o)

        printFileDiff :: GitFileContent -> IO ()
        printFileDiff NewBinaryFile = putStrLn "Binary file created"
        printFileDiff OldBinaryFile = putStrLn "Binary file deleted"
        printFileDiff ModifiedBinaryFile = putStrLn "Binary file modified"
        printFileDiff UnModifiedFile = putStrLn "No changes in the file's content"
        printFileDiff (NewTextFile l) = mapM_ (printFileLine "+") l
        printFileDiff (OldTextFile l) = mapM_ (printFileLine "-") l
        printFileDiff (ModifiedFile fDiff) = mapM_ printFilteredDiff fDiff

        printFilteredDiff :: FilteredDiff -> IO ()
        printFilteredDiff (NormalLine l) =
            case l of
                (Both (TextLine on ol) (TextLine nn _ )) -> printf "%4d %4d  %s\n" on nn (LC.unpack ol)
                (New                   (TextLine nn nl)) -> printf "     %4d +%s\n" nn (LC.unpack nl)
                (Old  (TextLine on ol)                 ) -> printf "%4d      -%s\n" on (LC.unpack ol)
        printFilteredDiff _ = putStrLn "           [...]"

        printFileLine :: String -> TextLine -> IO ()
        printFileLine prefix (TextLine _ line) = putStrLn $ prefix ++ (LC.unpack line)


showRefs git = do
    putStrLn "[BRANCHES]"
    heads <- branchList git
    mapM_ (putStrLn . refNameRaw) $ S.toList heads
    putStrLn "[TAGS]"
    tags <- tagList git
    mapM_ (putStrLn . refNameRaw) $ S.toList tags

main = do
    args <- getArgs
    case args of
        ["verify-pack",ref]  -> withCurrentRepo $ verifyPack (fromHexString @SHA1 ref)
        ["cat-file",ty,ref]  -> withCurrentRepo $ catFile ty (fromHexString ref)
        ["ls-tree",rev]      -> withCurrentRepo $ lsTree (fromString rev) ""
        ["ls-tree",rev,path] -> withCurrentRepo $ lsTree (fromString rev) path
        ["rev-list",rev]     -> withCurrentRepo $ revList (fromString rev)
        ["log",rev]          -> withCurrentRepo $ getLog (fromString rev)
        ["diff",rev1,rev2]   -> withCurrentRepo $ showDiff (fromString rev1) (fromString rev2)
        ["tag"]              -> withCurrentRepo $ showRefs
        ["show-refs"]        -> withCurrentRepo $ showRefs
        ["read-config"]      -> withCurrentRepo $ \git -> configGetAll git >>= putStrLn . show
        ["config",section,value] -> withCurrentRepo $ \git -> configGet git section value >>= putStrLn . show
        cmd : [] -> error ("unknown command: " ++ cmd)
        []       -> error "no args"
        _        -> error "unknown command line arguments"
