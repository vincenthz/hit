-- |
-- Module      : Data.Git.FileWriter
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
module Data.Git.FileWriter where

import Data.Git.Ref
import Data.IORef
import qualified Data.ByteString as B
import Codec.Zlib
import Control.Exception (bracket)

import qualified Crypto.Hash.SHA1 as SHA1

import System.IO

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

postDeflate _      Nothing    = return ()
postDeflate handle (Just dbs) = B.hPut handle dbs

fileWriterOutput (FileWriter { writerHandle = handle, writerDigest = digest, writerDeflate = deflate }) bs = do
	putStrLn ("outputing" ++ show bs)
	modifyIORef digest (\ctx -> SHA1.update ctx bs)
	postDeflate handle =<< withDeflateInput deflate bs id

fileWriterClose (FileWriter { writerHandle = handle, writerDeflate = deflate }) = do
	postDeflate handle =<< finishDeflate deflate id

fileWriterGetDigest (FileWriter { writerDigest = digest }) = (fromBinary . SHA1.finalize) `fmap` readIORef digest
