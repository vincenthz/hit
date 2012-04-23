-- |
-- Module      : Data.Git.Index
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Data.Git.Index
        ( IndexHeader(..)
        , Index(..)

        -- * handles and enumeration
        , indexOpen
        , indexClose
        , withIndex
        , indexEnumerate

        -- * read from index
        , indexHeaderGetNbWithPrefix
        , indexGetReferenceLocation
        , indexGetReferencesWithPrefix
        , indexReadHeader
        , indexRead
        , indexGetHeader
        ) where

import Control.Applicative ((<$>))
import Control.Monad

import System.FilePath
import System.Directory
import System.IO

import Data.List
import Data.Bits
import Data.Word

import Data.Vector (Vector, (!))
import qualified Data.Vector as V

import qualified Data.Attoparsec as A

import Data.Git.Internal
import Data.Git.FileReader
import Data.Git.Path
import Data.Git.Ref

-- | represent an index header with the version and the fanout table
data IndexHeader = IndexHeader !Word32 !(Vector Word32)
        deriving (Show,Eq)

data Index = Index
        { indexSha1s        :: Vector Ref
        , indexCRCs         :: Vector Word32
        , indexPackoffs     :: Vector Word32
        , indexPackChecksum :: Ref
        , indexChecksum     :: Ref
        }

-- | enumerate every indexes file in the pack directory
indexEnumerate repoPath = map onlyHash . filter isPackFile <$> getDirectoryContents (repoPath </> "objects" </> "pack")
        where
                isPackFile x = ".idx" `isSuffixOf` x && "pack-" `isPrefixOf` x
                onlyHash = fromHexString . takebut 4 . drop 5
                takebut n l = take (length l - n) l

-- | open an index
indexOpen :: FilePath -> Ref -> IO FileReader
indexOpen repoPath indexRef = openFile (indexPath repoPath indexRef) ReadMode >>= fileReaderNew False

-- | close an index
indexClose :: FileReader -> IO ()
indexClose = fileReaderClose

-- | variant of withFile on the index file and with a FileReader
withIndex repoPath indexRef = withFileReader (indexPath repoPath indexRef)

-- | returns the number of references, referenced in this index.
indexHeaderGetSize :: IndexHeader -> Word32
indexHeaderGetSize (IndexHeader _ indexes) = indexes ! 255

-- | byte size of an index header.
indexHeaderByteSize :: Int
indexHeaderByteSize = 2*4 {- header -} + 256*4 {- fanout table -}

-- | get the number of reference in this index with a specific prefix
indexHeaderGetNbWithPrefix :: IndexHeader -> Int -> Word32
indexHeaderGetNbWithPrefix (IndexHeader _ indexes) n
        | n < 0 || n > 255 = 0
        | n == 0           = indexes ! 0
        | otherwise        = (indexes ! n) - (indexes ! (n-1))

-- | fold on refs with a specific prefix
indexHeaderFoldRef :: IndexHeader -> FileReader -> Int -> (a -> Word32 -> Ref -> (a, Bool)) -> a -> IO a
indexHeaderFoldRef idxHdr@(IndexHeader _ indexes) fr refprefix f initAcc
        | nb == 0   = return initAcc
        | otherwise = do
                let spos = (indexes ! refprefix) - nb
                fileReaderSeek fr (fromIntegral (sha1Offset + spos * 20))
                loop nb initAcc
        where
                loop 0 acc = return acc
                loop n acc = do
                        b <- fromBinary <$> fileReaderGetBS 20 fr
                        let (!nacc, terminate) = f acc (nb-n) b
                        if terminate
                                then return nacc
                                else loop (n-1) nacc
                nb         = indexHeaderGetNbWithPrefix idxHdr refprefix
                (sha1Offset,_,_) = indexOffsets idxHdr

-- | return the reference offset in the packfile if found
indexGetReferenceLocation :: IndexHeader -> FileReader -> Ref -> IO (Maybe Word64)
indexGetReferenceLocation idxHdr@(IndexHeader _ indexes) fr ref = do
        mrpos <- indexHeaderFoldRef idxHdr fr refprefix f Nothing
        case mrpos of
                Nothing   -> return Nothing
                Just rpos -> do
                        let spos = (indexes ! refprefix) - nb
                        fileReaderSeek fr (fromIntegral (packOffset + 4 * (spos+rpos)))
                        Just . fromIntegral . be32 <$> fileReaderGetBS 4 fr
        where
                f acc rpos rref = if ref == rref then (Just rpos,True) else (acc,False)
                refprefix  = refPrefix ref
                nb         = indexHeaderGetNbWithPrefix idxHdr refprefix
                (_,_,packOffset) = indexOffsets idxHdr

-- | get all references that start by prefix.
indexGetReferencesWithPrefix :: IndexHeader -> FileReader -> String -> IO [Ref]
indexGetReferencesWithPrefix idxHdr fr prefix =
        indexHeaderFoldRef idxHdr fr refprefix f []
        where
                f acc _ ref = case cmpPrefix prefix ref of
                        GT -> (acc    ,False)
                        EQ -> (ref:acc,False)
                        LT -> (acc    ,True)
                refprefix   = read ("0x" ++ take 2 prefix)

-- | returns absolute offset in the index file of the sha1s, the crcs and the packfiles offset.
indexOffsets idx = (indexSha1sOffset, indexCRCsOffset, indexPackOffOffset)
        where
                indexPackOffOffset = indexCRCsOffset + crcsTableSz
                indexCRCsOffset    = indexSha1sOffset + sha1TableSz
                indexSha1sOffset   = fromIntegral indexHeaderByteSize
                crcsTableSz        = 4 * sz
                sha1TableSz        = 20 * sz
                sz                 = indexHeaderGetSize idx

-- | parse index header
parseIndexHeader = do
        magic   <- be32 <$> A.take 4
        when (magic /= 0xff744f63) $ error "wrong magic number for index"
        ver     <- be32 <$> A.take 4
        when (ver /= 2) $ error "unsupported index version"
        fanouts <- V.replicateM 256 (be32 <$> A.take 4)
        return $ IndexHeader ver fanouts

-- | read index header from an index filereader
indexReadHeader :: FileReader -> IO IndexHeader
indexReadHeader fr = fileReaderSeek fr 0 >> fileReaderParse fr parseIndexHeader

-- | get index header from an index reference
indexGetHeader :: FilePath -> Ref -> IO IndexHeader
indexGetHeader repoPath indexRef = withIndex repoPath indexRef $ indexReadHeader

-- | read all index
indexRead repoPath indexRef = do
        withIndex repoPath indexRef $ \fr -> do
                idx <- fileReaderParse fr parseIndexHeader
                liftM2 (,) (return idx) (fileReaderParse fr (parseIndex $ indexHeaderGetSize idx))
        where parseIndex sz = do
                sha1s     <- V.replicateM (fromIntegral sz) (fromBinary <$> A.take 20)
                crcs      <- V.replicateM (fromIntegral sz) (be32 <$> A.take 4)
                packoffs  <- V.replicateM (fromIntegral sz) (be32 <$> A.take 4)
                let nbLarge = length $ filter (== True) $ map (\packoff -> packoff `testBit` 31) $ V.toList packoffs
                largeoffs <- replicateM nbLarge (A.take 4)
                packfileChecksum <- fromBinary <$> A.take 20
                idxfileChecksum  <- fromBinary <$> A.take 20
                -- large packfile offsets
                -- trailer
                return (sha1s, crcs, packoffs, largeoffs, packfileChecksum, idxfileChecksum)
