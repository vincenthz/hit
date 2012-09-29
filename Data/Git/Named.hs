{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Git.Named
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
module Data.Git.Named
    ( RefSpecTy(..)
    , RefContentTy(..)
    , readPackedRefs
    , existsRefFile
    , writeRefFile
    , readRefFile
    ) where

import Control.Applicative ((<$>))

import qualified Filesystem as F
import qualified Filesystem.Path.Rules as FP (posix, decode, encode, encodeString, decodeString)
import Filesystem.Path.CurrentOS

import Data.String
import Data.Git.Path
import Data.Git.Ref
import Data.List (isPrefixOf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Prelude hiding (FilePath)

-- | Represent a named specifier.
data RefSpecTy = RefHead
               | RefOrigHead
               | RefFetchHead
               | RefBranch String
               | RefTag String
               | RefRemote String
               | RefPatches String
               | RefStash
               | RefOther String
               deriving (Show,Eq,Ord)

-- | content of a ref file.
data RefContentTy = RefDirect Ref
                  | RefLink   RefSpecTy
                  | RefContentUnknown B.ByteString
                  deriving (Show,Eq)

-- FIXME BC.unpack/pack should be probably be utf8.toString,
-- however i don't know if encoding is consistant.
-- it should probably be overridable.
pathDecode :: B.ByteString -> FilePath
pathDecode = FP.decode FP.posix

pathEncode :: FilePath -> B.ByteString
pathEncode = FP.encode FP.posix

toRefTy :: String -> RefSpecTy
toRefTy s
    | "refs/tags/" `isPrefixOf` s    = RefTag $ drop 10 s
    | "refs/heads/" `isPrefixOf` s   = RefBranch $ drop 11 s
    | "refs/remotes/" `isPrefixOf` s = RefRemote $ drop 13 s
    | "refs/patches/" `isPrefixOf` s = RefPatches $ drop 13 s
    | "refs/stash" == s              = RefStash
    | "HEAD" == s                    = RefHead
    | "ORIG_HEAD" == s               = RefOrigHead
    | "FETCH_HEAD" == s              = RefFetchHead
    | otherwise                      = RefOther $ s

fromRefTy :: RefSpecTy -> String
fromRefTy (RefBranch h)  = "refs/heads/" ++ h
fromRefTy (RefTag h)     = "refs/tags/" ++ h
fromRefTy (RefRemote h)  = "refs/remotes/" ++ h
fromRefTy (RefPatches h) = "refs/patches/" ++ h
fromRefTy RefStash       = "refs/stash"
fromRefTy RefHead        = "HEAD"
fromRefTy RefOrigHead    = "ORIG_HEAD"
fromRefTy RefFetchHead   = "FETCH_HEAD"
fromRefTy (RefOther h)   = h

toPath :: FilePath -> RefSpecTy -> FilePath
toPath gitRepo (RefBranch h)  = gitRepo </> "refs" </> "heads" </> fromString h
toPath gitRepo (RefTag h)     = gitRepo </> "refs" </> "tags" </> fromString h
toPath gitRepo (RefRemote h)  = gitRepo </> "refs" </> "remotes" </> fromString h
toPath gitRepo (RefPatches h) = gitRepo </> "refs" </> "patches" </> fromString h
toPath gitRepo RefStash       = gitRepo </> "refs" </> "stash"
toPath gitRepo RefHead        = gitRepo </> "HEAD"
toPath gitRepo RefOrigHead    = gitRepo </> "ORIG_HEAD"
toPath gitRepo RefFetchHead   = gitRepo </> "FETCH_HEAD"
toPath gitRepo (RefOther h)   = gitRepo </> fromString h

readPackedRefs gitRepo = do
    exists <- F.isFile (packedRefsPath gitRepo)
    if exists then readLines else return []
    where readLines = foldl accu [] . BC.lines <$> F.readFile (packedRefsPath gitRepo)
          accu a l
            | "#" `BC.isPrefixOf` l = a
            | otherwise = let (ref, r) = B.splitAt 40 l
                              name     = FP.encodeString FP.posix $ pathDecode $ B.tail r
                           in (toRefTy name, fromHex ref) : a

{-
headsList gitRepo = getDirectoryContentNoDots (headsPath gitRepo)
tagsList gitRepo = getDirectoryContentNoDots (tagsPath gitRepo)
remotesList gitRepo = getDirectoryContentNoDots (remotesPath gitRepo)
remoteList gitRepo remote = getDirectoryContentNoDots (remotePath gitRepo remote)

writeRef path ref = B.writeFile path (B.concat [toHex ref, B.singleton 0xa])
readRef path = fromHex . B.take 40 <$> B.readFile path
-}

existsRefFile :: FilePath -> RefSpecTy -> IO Bool
existsRefFile gitRepo specty = F.isFile $ toPath gitRepo specty

writeRefFile :: FilePath -> RefSpecTy -> RefContentTy -> IO ()
writeRefFile gitRepo specty refcont = F.writeFile filepath $ fromRefContent refcont
    where filepath = toPath gitRepo specty
          fromRefContent (RefLink link)        = B.concat ["ref: ", pathEncode $ FP.decodeString FP.posix $ fromRefTy link, B.singleton 0xa]
          fromRefContent (RefDirect ref)       = B.concat [toHex ref, B.singleton 0xa]
          fromRefContent (RefContentUnknown c) = c

readRefFile :: FilePath -> RefSpecTy -> IO RefContentTy
readRefFile gitRepo specty = toRefContent <$> F.readFile filepath
    where filepath = toPath gitRepo specty
          toRefContent content
            | "ref: " `B.isPrefixOf` content = RefLink $ toRefTy $ FP.encodeString FP.posix $ pathDecode $ head $ BC.lines $ B.drop 5 content
            | B.length content < 42          = RefDirect $ fromHex $ B.take 40 content
            | otherwise                      = RefContentUnknown content

{-
headExists gitRepo name    = doesFileExist (headPath gitRepo name)
headRead gitRepo name      = readRef (headPath gitRepo name)
headWrite gitRepo name ref = writeRef (headPath gitRepo name) ref

tagExists gitRepo name    = doesFileExist (tagPath gitRepo name)
tagRead gitRepo name      = readRef (tagPath gitRepo name)
tagWrite gitRepo name ref = writeRef (tagPath gitRepo name) ref

specialRead gitRepo name   = readRefAndFollow gitRepo (specialPath gitRepo name)
specialExists gitRepo name = doesFileExist (specialPath gitRepo name)
-}
