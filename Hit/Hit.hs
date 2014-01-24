{-# LANGUAGE BangPatterns #-}
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
import Data.Word
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Char8 as BC
import Text.Printf
import qualified Data.Map as M
import qualified Data.HashTable.IO as H
import qualified Data.Hashable as Hashable

import Data.Algorithm.Patience as AP (Item(..))

type HashTable k v = H.CuckooHashTable k v

instance Hashable.Hashable Ref where
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

        dumpTree :: HashTable Word64 Ref -> HashTable Ref (PackedObjectInfo, Maybe (ObjectPtr, Int)) -> M.Map Ref () -> IO ()
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
        showTreeEnt (p,n,TreeDir r _) = printf "%06o tree %s    %s\n" p (show r) (BC.unpack n)
        showTreeEnt (p,n,TreeFile r)  = printf "%06o blob %s    %s\n" p (show r) (BC.unpack n)

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
                , ("date:   " ++ show (toZonedTime $ personTime author) ++ " (" ++ show (toUTCTime $ personTime author) ++ ")")
                , ""
                , BC.unpack $ commitMessage commit
                ]
            return ()
          where author = commitAuthor commit

showDiff rev1 rev2 git = do
    diffList <- getDiff rev1 rev2 git
    mapM_ showADiff diffList
    where
        showADiff :: HitDiff -> IO ()
        showADiff hd = do
            let filename = BC.unpack $ hitFilename hd
            putStrLn $ "Diff --hit old/" ++ filename ++ " new/" ++ filename
            ppHitMode $ hitDiff hd
            ppHitRefs $ hitDiff hd
            ppHitDiff $ hitDiff hd

        ppHitMode :: [HitDiffContent] -> IO ()
        ppHitMode []                         = return ()
        ppHitMode ((HitDiffMode old new):_ ) = printf "old mode %06o\nnew mode %06o\n" old new
        ppHitMode (_                    :xs) = ppHitMode xs

        ppHitRefs :: [HitDiffContent] -> IO ()
        ppHitRefs []                         = return ()
        ppHitRefs ((HitDiffRefs old new):_ ) = printf "--- old: %s\n+++ new: %s\n" (show old) (show new)
        ppHitRefs ((HitDiffAddition new):_ ) = printf "--- old: dev/null\n+++ new: %s\n" (show $ bsRef new)
        ppHitRefs ((HitDiffDeletion old):_ ) = printf "--- old: %s\n+++ new: dev/null\n" (show $ bsRef old)
        ppHitRefs (_                    :xs) = ppHitRefs xs

        ppHitDiff :: [HitDiffContent] -> IO ()
        ppHitDiff []                         = return ()
        ppHitDiff ((HitDiffAddition new):_ ) =
            case bsContent new of
                FileContent content -> printf "%s" (LC.unpack $ LC.concat $ doPPDiffLine "+" content)
                _                   -> return ()
        ppHitDiff ((HitDiffDeletion old):_ ) =
            case bsContent old of
                FileContent content -> printf "%s" (LC.unpack $ LC.concat $ doPPDiffLine "-" content)
                _                   -> return ()
        ppHitDiff ((HitDiffChange   l  ):_ ) = printf "%s" (LC.unpack $ LC.concat $ doPPDiff l)
        ppHitDiff ((HitDiffBinChange   ):_ ) = putStrLn "Binary files differ"
        ppHitDiff (_                    :xs) = ppHitDiff xs

        doPPDiff :: [Item LC.ByteString] -> [LC.ByteString]
        doPPDiff []              = []
        doPPDiff ((Both a _):xs) = (doPPDiffLine " " [a]) ++ (doPPDiff xs)
        doPPDiff ((Old  a  ):xs) = (doPPDiffLine "-" [a]) ++ (doPPDiff xs)
        doPPDiff ((New    b):xs) = (doPPDiffLine "+" [b]) ++ (doPPDiff xs)

        doPPDiffLine :: String -> [LC.ByteString] -> [LC.ByteString]
        doPPDiffLine _      []     = []
        doPPDiffLine prefix (a:xs) = (LC.concat [LC.pack prefix,a,LC.pack "\n"]):(doPPDiffLine prefix xs)

main = do
    args <- getArgs
    case args of
        ["verify-pack",ref]  -> withCurrentRepo $ verifyPack (fromHexString ref)
        ["cat-file",ty,ref]  -> withCurrentRepo $ catFile ty (fromHexString ref)
        ["ls-tree",rev]      -> withCurrentRepo $ lsTree (fromString rev) ""
        ["ls-tree",rev,path] -> withCurrentRepo $ lsTree (fromString rev) path
        ["rev-list",rev]     -> withCurrentRepo $ revList (fromString rev)
        ["log",rev]          -> withCurrentRepo $ getLog (fromString rev)
        ["diff",rev1,rev2]   -> withCurrentRepo $ showDiff (fromString rev1) (fromString rev2)
        cmd : [] -> error ("unknown command: " ++ cmd)
        []       -> error "no args"
        _        -> error "unknown command line arguments"
