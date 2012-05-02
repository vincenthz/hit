-- |
-- Module      : Data.Git.Loose
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Data.Git.Loose
        (
        -- * marshall from and to lazy bytestring
          looseUnmarshall
        , looseUnmarshallRaw
        , looseMarshall
        -- * read and check object existence
        , looseRead
        , looseReadHeader
        , looseReadRaw
        , looseExists
        -- * write objects
        , looseWriteBlobFromFile
        , looseWrite
        -- * enumeration of loose objects
        , looseEnumeratePrefixes
        , looseEnumerateWithPrefixFilter
        , looseEnumerateWithPrefix
        ) where

import Codec.Compression.Zlib
import Data.Git.Ref
import Data.Git.Path
import Data.Git.FileWriter
import Data.Git.Object

import System.FilePath
import System.Directory
import System.IO (openFile, hFileSize, IOMode(..))

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L

import Data.Attoparsec.Lazy
import qualified Data.Attoparsec.Char8 as PC
import Control.Applicative ((<$>), (<|>))
import Control.Monad
import Control.Exception (onException, SomeException)
import qualified Control.Exception as E

import Data.Char (isHexDigit)

isObjectPrefix [a,b] = isHexDigit a && isHexDigit b
isObjectPrefix _     = False

decimal :: Parser Int
decimal = PC.decimal

-- loose object parsing
parseHeader = do
        h <- takeWhile1 ((/=) 0x20)
        _ <- word8 0x20
        sz <- decimal
        return (objectTypeUnmarshall $ BC.unpack h, fromIntegral sz, Nothing)

parseTreeHeader   = string "tree " >> decimal >> word8 0
parseTagHeader    = string "tag " >> decimal >> word8 0
parseCommitHeader = string "commit " >> decimal >> word8 0
parseBlobHeader   = string "blob " >> decimal >> word8 0

parseTree   = parseTreeHeader >> objectParseTree
parseTag    = parseTagHeader >> objectParseTag
parseCommit = parseCommitHeader >> objectParseCommit
parseBlob   = parseBlobHeader >> objectParseBlob

parseObject :: L.ByteString -> Object
parseObject = parseSuccess (parseTree <|> parseBlob <|> parseCommit <|> parseTag)
        where parseSuccess p = either error id . eitherResult . parse p

-- | unmarshall an object (with header) from a lazy bytestring.
looseUnmarshall :: L.ByteString -> Object
looseUnmarshall = parseObject . decompress

-- | unmarshall an object as (header, data) tuple from a lazy bytestring.
looseUnmarshallRaw :: L.ByteString -> (ObjectHeader, ObjectData)
looseUnmarshallRaw l =
        let dl = decompress l in
        let i = L.findIndex ((==) 0) dl in
        case i of
                Nothing  -> error "object not right format. missing 0"
                Just idx ->
                        let (h, r) = L.splitAt (idx+1) dl in
                        case maybeResult $ parse parseHeader h of
                                Nothing  -> error "cannot open object"
                                Just hdr -> (hdr, r)

-- | read a specific ref from a loose object and returns an header and data.
looseReadRaw repoPath ref = looseUnmarshallRaw <$> L.readFile (objectPathOfRef repoPath ref)

-- | read only the header of a loose object.
looseReadHeader repoPath ref = toHeader <$> L.readFile (objectPathOfRef repoPath ref)
        where toHeader = either error id . eitherResult . parse parseHeader . decompress

-- | read a specific ref from a loose object and returns an object
looseRead repoPath ref = looseUnmarshall <$> L.readFile (objectPathOfRef repoPath ref)

-- | check if a specific ref exists as loose object
looseExists repoPath ref = doesFileExist (objectPathOfRef repoPath ref)

-- | enumarate all prefixes available in the object store.
looseEnumeratePrefixes repoPath = filter isObjectPrefix <$> getDirectoryContents (repoPath </> "objects")

-- | enumerate all references available with a specific prefix.
looseEnumerateWithPrefixFilter :: FilePath -> String -> (Ref -> Bool) -> IO [Ref]
looseEnumerateWithPrefixFilter repoPath prefix filterF =
        filter filterF . map (fromHexString . (prefix ++)) . filter isRef <$> getDir (repoPath </> "objects" </> prefix)
        where
                getDir p = E.catch (getDirectoryContents p) (\(_::SomeException) -> return [])
                isRef l = length l == 38

looseEnumerateWithPrefix repoPath prefix =
        looseEnumerateWithPrefixFilter repoPath prefix (const True)

-- | marshall as lazy bytestring an object except deltas.
looseMarshall obj
        | objectIsDelta obj = error "cannot write delta object loose"
        | otherwise         = L.concat [ L.fromChunks [hdrB], objData ]
        where
                objData = objectWrite obj
                hdrB    = objectWriteHeader (objectToType obj) (fromIntegral $ L.length objData)

-- | create a new blob on a temporary location and on success move it to
-- the object store with its digest name.
looseWriteBlobFromFile repoPath file = do
        fsz <- openFile file ReadMode >>= hFileSize
        let hdr = objectWriteHeader TypeBlob (fromIntegral fsz)
        tmpPath <- objectTemporaryPath repoPath
        flip onException (removeFile tmpPath) $ do
                (ref, npath) <- withFileWriter tmpPath $ \fw -> do
                        fileWriterOutput fw hdr
                        chunks <- L.toChunks <$> L.readFile file
                        mapM_ (fileWriterOutput fw) chunks
                        digest <- fileWriterGetDigest fw
                        return (digest, objectPathOfRef repoPath digest)
                exists <- doesFileExist npath
                when exists $ error "destination already exists"
                renameFile tmpPath npath
                return ref

-- | write an object to disk as a loose reference.
-- use looseWriteBlobFromFile for efficiently writing blobs when being commited from a file.
looseWrite repoPath obj = createDirectoryIfMissing True (takeDirectory path)
                       >> doesFileExist path
                       >>= \exists -> unless exists (L.writeFile path $ compress content)
                       >> return ref
        where
                path    = objectPathOfRef repoPath ref
                content = looseMarshall obj
                ref     = hashLBS content
