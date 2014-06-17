-- |
-- Module      : Data.Git.Storage.FileBackend
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
-- Normal git storage with files
{-# LANGUAGE OverloadedStrings #-}
module Data.Git.Storage.FileBackend
    ( FileBackend
    , gitFilePath
    , newFileBackend
    , closeFileBackend
    , getObjectRawAt
    , getObjectAt
    -- * iterators
    , findReference
    ) where

import Data.Git.Ref
import Data.Git.Named
import Data.Git.Delta
import qualified Data.Git.Config as Cfg
import Data.Git.Path (packedRefsPath)
import Data.Git.Storage.Backend
import Data.Git.Storage.FileReader
import Data.Git.Storage.PackIndex
import Data.Git.Storage.Object
import Data.Git.Storage.Pack
import Data.Git.Storage.Loose
import Data.Git.Storage.CacheFile

import Filesystem
import Filesystem.Path hiding (concat)
import Filesystem.Path.Rules

import Control.Applicative ((<$>), (<*>))
import Control.Monad

import Data.String
import Data.List ((\\), isPrefixOf)
import Data.IORef
import Data.Word
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as M
import Prelude hiding (FilePath)

data PackIndexReader = PackIndexReader PackIndexHeader FileReader

-- | this is a cache representation of the packed-ref file
type CachedPackedRef = CacheFile (PackedRefs (M.Map RefName Ref))

data FileBackend = FileBackend
    { gitFilePath  :: FilePath
    , indexReaders :: IORef [(Ref, PackIndexReader)]
    , packReaders  :: IORef [(Ref, FileReader)]
    , packedNamed  :: CachedPackedRef
    }

gitRepoPath = gitFilePath

newFileBackend :: FilePath -> IO FileBackend
newFileBackend path =
    FileBackend path <$> newIORef [] <*> newIORef [] <*> packedRef
  where packedRef = newCacheVal (packedRefsPath path)
                                (readPackedRefs path M.fromList)
                                (PackedRefs M.empty M.empty M.empty)

closeFileBackend :: FileBackend -> IO ()
closeFileBackend backend = do
    mapM_ (closeIndexReader . snd) =<< readIORef ireaders
    mapM_ (fileReaderClose . snd) =<< readIORef preaders
  where closeIndexReader (PackIndexReader _ fr) = fileReaderClose fr
        (FileBackend { indexReaders = ireaders, packReaders = preaders }) = backend

fbBranchWrite git branchName ref =
    writeRefFile (gitRepoPath git) (RefBranch branchName) (RefDirect ref)
fbBranchList git = do
    ps <- Set.fromList . M.keys . packedBranchs <$> getCacheVal (packedNamed git)
    ls <- Set.fromList <$> looseHeadsList (gitRepoPath git)
    return $ Set.union ps ls
fbTagWrite :: FileBackend -> RefName -> Ref -> IO ()
fbTagWrite git tagname ref =
    writeRefFile (gitRepoPath git) (RefTag tagname) (RefDirect ref)

fbTagList :: FileBackend -> IO (Set RefName)
fbTagList git = do
    ps <- Set.fromList . M.keys . packedTags <$> getCacheVal (packedNamed git)
    ls <- Set.fromList <$> looseTagsList (gitRepoPath git)
    return $ Set.union ps ls

-- | Set head to point to either a reference or a branch name.
fbHeadSet :: FileBackend -> Either Ref RefName -> IO ()
fbHeadSet git (Left ref)      =
    writeRefFile (gitRepoPath git) RefHead (RefDirect ref)
fbHeadSet git (Right refname) =
    writeRefFile (gitRepoPath git) RefHead (RefLink $ RefBranch refname)

-- | Get what the head is pointing to, or the reference otherwise
fbHeadGet :: FileBackend
          -> IO (Either Ref RefName)
fbHeadGet git = do
    content <- readRefFile (gitRepoPath git) RefHead
    case content of
        RefLink (RefBranch b) -> return $ Right b
        RefLink spec          -> error ("unknown content link in HEAD: " ++ show spec)
        RefDirect r           -> return $ Left r
        RefContentUnknown bs  -> error ("unknown content in HEAD: " ++ show bs)

getObjectRawFile gfb ref resolveDelta = do
    loc <- findReference gfb ref
    getObjectRawAt gfb loc resolveDelta

-- | get an object type from repository
getObjectType :: FileBackend -> Ref -> IO (Maybe ObjectType)
getObjectType git ref = findReference git ref >>= getObjectTypeAt
  where getObjectTypeAt NotFound        = return Nothing
        getObjectTypeAt (Loose _)       = Just . (\(t,_,_) -> t) <$> looseReadHeader (gitFilePath git) ref
        getObjectTypeAt (Packed pref o) = fmap ((\(ty,_,_) -> ty) . oiHeader) <$> readFromPack git pref o True

-- | get an object from repository
getObjectRawAt :: FileBackend -> ObjectLocation -> Bool -> IO (Maybe ObjectInfo)
getObjectRawAt _   NotFound    _ = return Nothing
getObjectRawAt git (Loose ref) _ = Just . (\(h,d)-> ObjectInfo h d[]) <$> looseReadRaw (gitFilePath git) ref
getObjectRawAt git (Packed pref o) resolveDelta = readFromPack git pref o resolveDelta

-- | get an object from repository using a location to reference it.
getObjectAt :: FileBackend -> ObjectLocation -> Bool -> IO (Maybe Object)
getObjectAt git loc resolveDelta = maybe Nothing toObj <$> getObjectRawAt git loc resolveDelta
  where toObj (ObjectInfo { oiHeader = (ty, _, extra), oiData = objData }) = packObjectFromRaw (ty, extra, objData)

-- | read the repository's description
getDescription :: FileBackend -> IO (Maybe String)
getDescription git = do
    isdescription <- isFile descriptionPath
    if (isdescription)
        then do
                content <- Prelude.readFile $ encodeString posix descriptionPath
                return $ Just content
        else return Nothing
  where descriptionPath = gitFilePath git </> "description"

-- | set the repository's description
setDescription :: FileBackend -> String -> IO ()
setDescription git desc = do
    Prelude.writeFile (encodeString posix descriptionPath) desc
  where descriptionPath = gitFilePath git </> "description"


-- | Get the object location of a specific reference
findReference :: FileBackend -> Ref -> IO ObjectLocation
findReference git ref = maybe NotFound id <$> (findLoose `mplusIO` findInIndexes)
  where findLoose :: IO (Maybe ObjectLocation)
        findLoose = do
            isLoose <- looseExists (gitFilePath git) ref
            if isLoose then return (Just $ Loose ref) else return Nothing

        findInIndexes :: IO (Maybe ObjectLocation)
        findInIndexes = iterateIndexes git isinIndex Nothing --f -> (a -> IndexReader -> IO (a,Bool)) -> a -> IO a

        isinIndex acc (idxref, (PackIndexReader idxhdr indexreader)) = do
            mloc <- packIndexGetReferenceLocation idxhdr indexreader ref
            case mloc of
                Nothing  -> return (acc, False)
                Just loc -> return (Just $ Packed idxref loc, True)

        mplusIO :: IO (Maybe a) -> IO (Maybe a) -> IO (Maybe a)
        mplusIO f g = f >>= \vopt -> case vopt of
            Nothing -> g
            Just v  -> return $ Just v

-- | get all the references that start by a specific prefix
findReferencesWithPrefix :: FileBackend -> String -> IO [Ref]
findReferencesWithPrefix git pre
    | invalidLength         = error ("not a valid prefix: " ++ show pre)
    | not (isHexString pre) = error ("reference prefix contains non hexchar: " ++ show pre)
    | otherwise             = do
        looseRefs  <- looseEnumerateWithPrefixFilter (gitFilePath git) (take 2 pre) matchRef
        packedRefs <- concat <$> iterateIndexes git idxPrefixMatch []
        return (looseRefs ++ packedRefs)
  where -- not very efficient way to do that... will do for now.
        matchRef ref = pre `isPrefixOf` toHexString ref
        invalidLength = length pre < 2 || length pre > 39 

        idxPrefixMatch acc (_, (PackIndexReader idxhdr indexreader)) = do
            refs <- packIndexGetReferencesWithPrefix idxhdr indexreader pre
            return (refs:acc,False)

iterateIndexes git f initAcc = do
    allIndexes    <- packIndexEnumerate (gitFilePath git)
    readers       <- readIORef (indexReaders git)
    (a,terminate) <- loop initAcc readers
    if terminate
        then return a
        else readRemainingIndexes a (allIndexes \\ map fst readers)
  where loop acc []     = return (acc, False)
        loop acc (r:rs) = do
            (nacc, terminate) <- f acc r
            if terminate
                then return (nacc,True)
                else loop nacc rs

        readRemainingIndexes acc []            = return acc
        readRemainingIndexes acc (idxref:idxs) = do
            fr <- packIndexOpen (gitFilePath git) idxref
            idx <- packIndexReadHeader fr
            let idxreader = PackIndexReader idx fr
            let r = (idxref, idxreader)
            modifyIORef (indexReaders git) (\l -> r : l)
            (nacc, terminate) <- f acc r
            if terminate
                then return nacc
                else readRemainingIndexes nacc idxs


readRawFromPack :: FileBackend -> Ref -> Word64 -> IO (FileReader, PackedObjectRaw)
readRawFromPack git pref offset = do
    readers <- readIORef (packReaders git)
    reader  <- maybe getDefault return $ lookup pref readers
    po <- packReadRawAtOffset reader offset
    return (reader, po)
  where getDefault = do p <- packOpen (gitFilePath git) pref
                        modifyIORef (packReaders git) ((pref, p):)
                        return p

readFromPack :: FileBackend -> Ref -> Word64 -> Bool -> IO (Maybe ObjectInfo)
readFromPack git pref o resolveDelta = do
    (reader, x) <- readRawFromPack git pref o
    if resolveDelta then resolve reader o x else return $ Just $ generifyHeader x
  where generifyHeader :: PackedObjectRaw -> ObjectInfo
        generifyHeader (po, objData) = ObjectInfo { oiHeader = hdr, oiData = objData, oiChains = [] }
          where hdr = (poiType po, poiActualSize po, poiExtra po)

        resolve :: FileReader -> Word64 -> PackedObjectRaw -> IO (Maybe ObjectInfo)
        resolve reader offset (po, objData) = do
            case (poiType po, poiExtra po) of
                (TypeDeltaOff, Just ptr@(PtrOfs doff)) -> do
                    let delta = deltaRead objData
                    let noffset = offset - doff
                    base <- resolve reader noffset =<< packReadRawAtOffset reader noffset
                    return $ addToChain ptr $ applyDelta delta base
                (TypeDeltaRef, Just ptr@(PtrRef bref)) -> do
                    let delta = deltaRead objData
                    base <- getObjectRawFile git bref True
                    return $ addToChain ptr $ applyDelta delta base
                _ ->
                    return $ Just $ generifyHeader (po, objData)

        addToChain ptr (Just oi) = Just (oi { oiChains = ptr : oiChains oi })
        addToChain _   Nothing   = Nothing

        applyDelta :: Maybe Delta -> Maybe ObjectInfo -> Maybe ObjectInfo
        applyDelta (Just delta@(Delta _ rSize _)) (Just objInfo) = Just $ objInfo
            { oiHeader = (\(a,_,c) -> (a,rSize,c)) $ oiHeader objInfo
            , oiData   = deltaApply (oiData objInfo) delta
            }
        applyDelta _ _                                      = Nothing

fbRefRead git refty = do
    exists <- existsRefFile (gitFilePath git) refty
    if exists
        then do refcont <- readRefFile (gitFilePath git) refty
                case refcont of
                     RefDirect ref     -> return $ Just ref
                     RefLink refspecty -> fbRefRead git refspecty
                     _                 -> error "cannot handle reference content"
        else do lookupCache <- getCacheVal $ packedNamed git
                case refty of
                    RefTag name    -> mapLookup name $ packedTags lookupCache
                    RefBranch name -> mapLookup name $ packedBranchs lookupCache
                    RefRemote name -> mapLookup name $ packedRemotes lookupCache
                    _              -> return Nothing
  where mapLookup name m = return $ M.lookup name m

instance GitStorage FileBackend where
    closeStorage = closeFileBackend

    objectGetRaw = getObjectRawFile
    objectSet    = looseWrite . gitRepoPath
    objectType   = getObjectType
    objectFindPrefix = findReferencesWithPrefix

    -- description
    descriptionGet = getDescription
    descriptionSet = setDescription

    configGet git = Cfg.readConfig (gitRepoPath git)

    refRead = fbRefRead

    branchList = fbBranchList
    branchWrite = fbBranchWrite
    tagList = fbTagList
    tagWrite = fbTagWrite

    headGet = fbHeadGet
    headSet = fbHeadSet
