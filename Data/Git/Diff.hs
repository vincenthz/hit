-- |
-- Module      : Data.Git.Diff
-- License     : BSD-style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unix
--

module Data.Git.Diff
    (
    -- * Basic features
      BlobContent(..)
    , BlobState(..)
    , BlobStateDiff(..)
    , getDiffWith
    -- * Default helpers
    , HitDiffContent(..)
    , HitDiff(..)
    , defaultDiff
    , getDiff
    ) where

import Control.Applicative ((<$>))

import Data.List (find, filter)
import Data.Char (ord)
import Data.Git
import Data.Git.Repository
import Data.Git.Storage
import Data.Git.Storage.Object
import Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Char8 as BS

import Data.Algorithm.Patience as AP (Item(..), diff)

data BlobContent = FileContent [L.ByteString] | BinaryContent L.ByteString
    deriving (Show)

-- | This is a blob description.
data BlobState = BlobState
    { bsFilename :: BS.ByteString
    , bsMode     :: Int
    , bsRef      :: Ref
    , bsContent  :: BlobContent
    }
    deriving (Show)
instance Eq BlobState where
    (BlobState f1 _ _ _) == (BlobState f2 _ _ _) = (f2 == f1)
    a /= b = not (a == b)

-- | Represents a file state between two references
data BlobStateDiff = OnlyOld BlobState
                   | OnlyNew BlobState
                   | OldAndNew  BlobState BlobState

getBinaryStat :: L.ByteString -> Double
getBinaryStat bs = L.foldl' (\acc w -> acc + if isBin $ ord w then 1 else 0) 0 bs / (fromIntegral $ L.length bs)
    where
        isBin :: Int -> Bool
        isBin i
            | i >= 0 && i <= 8   = True
            | i == 12            = True
            | i >= 14 && i <= 31 = True
            | otherwise          = False

isBinaryFile :: L.ByteString -> Bool
isBinaryFile file =
    let bs = L.take 512 file
    in  getBinaryStat bs > 0.0

buildListForDiff :: Git -> Revision -> IO [BlobState]
buildListForDiff git revision = do
    ref    <- maybe (error "revision cannot be found") id <$> resolveRevision git revision
    commit <- getCommit git ref
    tree   <- resolveTreeish git $ commitTreeish commit
    case tree of
        Just t -> do htree <- buildHTree git t
                     buildTreeList htree (BS.empty)
        _      -> error "cannot build a tree from this reference"
    where
        buildTreeList :: HTree -> BS.ByteString -> IO [BlobState]
        buildTreeList [] _ = return []
        buildTreeList ((d,n,TreeFile r):xs)  pathPrefix = do
            content <- catBlobFile r
            let isABinary = isBinaryFile content
            listTail <- buildTreeList xs pathPrefix
            case isABinary of
                False -> return $ (BlobState (BS.append pathPrefix n) d r (FileContent $ L.lines content)) : listTail
                True  -> return $ (BlobState (BS.append pathPrefix n) d r (BinaryContent content)) : listTail
        buildTreeList ((_,n,TreeDir _ subTree):xs) pathPrefix = do
            l1 <- buildTreeList xs      pathPrefix
            l2 <- buildTreeList subTree (BS.concat [pathPrefix, n, BS.pack "/"])
            return $ l1 ++ l2

        catBlobFile :: Ref -> IO L.ByteString
        catBlobFile ref = do
            mobj <- getObjectRaw git ref True
            case mobj of
                Nothing  -> error "not a valid object"
                Just obj -> return $ oiData obj

-- | generate a diff list between two revisions with a given diff helper.
-- Useful to extract any kind of information from two different revisions:
-- For example you can get the number of deleted files:
--    getDiffWith f 0 HEAD^ HEAD git
--    where f (OnlyOld _) acc = acc+1
--          f _           acc = acc
-- you even can get a 'full' diff: see defaultDiff
getDiffWith :: (BlobStateDiff -> a -> a) -- ^ diff helper (State -> accumulator -> accumulator)
            -> a                         -- ^ accumulator
            -> Revision                  -- ^ commit revision
            -> Revision                  -- ^ commit revision
            -> Git                       -- ^ repository
            -> IO a
getDiffWith f acc rev1 rev2 git = do
    commit1 <- buildListForDiff git rev1
    commit2 <- buildListForDiff git rev2
    return $ Prelude.foldr f acc $ doDiffWith commit1 commit2
    where
        doDiffWith :: [BlobState] -> [BlobState] -> [BlobStateDiff]
        doDiffWith []        []        = []
        doDiffWith [bs1]     []        = [OnlyOld bs1]
        doDiffWith []        (bs2:xs2) = (OnlyNew bs2):(doDiffWith [] xs2)
        doDiffWith (bs1:xs1) xs2       =
            let bs2Maybe = Data.List.find (\x -> x == bs1) xs2
            in  case bs2Maybe of
                    Just bs2 -> let subxs2 = Data.List.filter (\x -> x /= bs2) xs2
                                in  (OldAndNew bs1 bs2):(doDiffWith xs1 subxs2)
                    Nothing  -> (OnlyOld bs1):(doDiffWith xs1 xs2)



-- | This is an example of how you can use Hit to get all of information
-- between different revision.
data HitDiffContent = HitDiffAddition  BlobState
                    | HitDiffDeletion  BlobState
                    | HitDiffChange    [AP.Item L.ByteString]
                    | HitDiffBinChange
                    | HitDiffMode      Int Int
                    | HitDiffRefs      Ref Ref
    deriving (Show)

-- | This represents a diff.
data HitDiff = HitDiff
    { hitFilename :: BS.ByteString
    , hitDiff     :: [HitDiffContent]
    } deriving (Show)

-- | A default Diff getter which returns all diff information (Mode, Content
-- and Binary).
-- gitDiff = getDiffWith defaultDiff
getDiff :: Revision -- ^ commit revision
        -> Revision -- ^ commit revision
        -> Git      -- ^ repository
        -> IO [HitDiff]
getDiff = getDiffWith defaultDiff []

-- | A default diff helper. It is an example about how you can write your own
-- diff helper or you can use it if you want to get all of differences.
defaultDiff :: BlobStateDiff -> [HitDiff] -> [HitDiff]
defaultDiff (OnlyOld   old    ) acc = (HitDiff (bsFilename old) ([HitDiffDeletion old])):acc
defaultDiff (OnlyNew       new) acc = (HitDiff (bsFilename new) ([HitDiffAddition new])):acc
defaultDiff (OldAndNew old new) acc =
    let theDiffMode = if (bsMode old) == (bsMode new) then [] else [HitDiffMode (bsMode old) (bsMode new)] in
    case ((bsRef old) == (bsRef new)) of
        -- If the reference is the same, then there is no difference
        True  -> if Prelude.null theDiffMode
                 then acc
                 else (HitDiff (bsFilename old) theDiffMode):acc
        False -> let theDiff = createANewDiff (bsContent old) (bsContent new) in
                 if (onlyBothDiff $ Prelude.head theDiff)
                 then (HitDiff (bsFilename old) ((HitDiffRefs (bsRef old) (bsRef new)):theDiffMode)):acc
                 else (HitDiff (bsFilename old) ( theDiff ++ ((HitDiffRefs (bsRef old) (bsRef new)):theDiffMode))):acc
    where
        createANewDiff :: BlobContent -> BlobContent -> [HitDiffContent]
        createANewDiff (FileContent   a) (FileContent   b) = [HitDiffChange (diff a b)]
        createANewDiff (BinaryContent a) (BinaryContent b) = if a /= b then [HitDiffBinChange] else []
        createANewDiff _                 _                 = [HitDiffBinChange]

        onlyBothDiff :: HitDiffContent -> Bool
        onlyBothDiff (HitDiffBinChange) = False
        onlyBothDiff (HitDiffChange l)  = Prelude.all predicate l
            where predicate (Both _ _) = True
                  predicate _          = False
        onlyBothDiff _                  = True
