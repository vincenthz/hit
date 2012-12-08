-- |
-- Module      : Data.Git.Storage.FileWriter
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
module Data.Git.Storage.FileWriter where

import Data.Git.Ref
import Data.IORef
import qualified Data.ByteString as B
import Codec.Zlib
import Control.Exception (bracket)

import qualified Crypto.Hash.SHA1 as SHA1

import System.IO (hClose)
import Filesystem

defaultCompression = 6

data FileWriter = FileWriter
        { writerHandle  :: Handle
        , writerDeflate :: Deflate
        , writerDigest  :: IORef SHA1.Ctx
        }

fileWriterNew handle = do
        deflate <- initDeflate defaultCompression defaultWindowBits
        digest  <- newIORef SHA1.init
        return $ FileWriter
                { writerHandle  = handle
                , writerDeflate = deflate
                , writerDigest  = digest
                }

withFileWriter path f =
        bracket (openFile path WriteMode) (hClose) $ \handle ->
                bracket (fileWriterNew handle) (fileWriterClose) f

postDeflate handle = maybe (return ()) (B.hPut handle)

fileWriterOutput (FileWriter { writerHandle = handle, writerDigest = digest, writerDeflate = deflate }) bs = do
        modifyIORef' digest (\ctx -> SHA1.update ctx bs)
        (>>= postDeflate handle) =<< feedDeflate deflate bs

fileWriterClose (FileWriter { writerHandle = handle, writerDeflate = deflate }) =
        postDeflate handle =<< finishDeflate deflate

fileWriterGetDigest (FileWriter { writerDigest = digest }) = (fromBinary . SHA1.finalize) `fmap` readIORef digest
