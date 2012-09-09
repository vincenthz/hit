{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Data.Git.Named
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
module Data.Git.Named
        ( headsList
        , headExists
        , headRead
        , headWrite
        , remotesList
        , remoteList
        , tagsList
        , tagExists
        , tagRead
        , tagWrite
        , specialRead
        , specialExists
        ) where

import Control.Applicative ((<$>))

import System.Directory

import Data.Git.Path
import Data.Git.Ref
import Data.List (isPrefixOf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

-- | Represent a named ref type.
data RefTy = RefTyHead String
           | RefTyTag String
           | RefTyRemote String
           | RefTyPatches String
           | RefTyStash
           | RefTyOther String
           deriving (Show,Eq,Ord)

-- FIXME BC.unpack/pack should be probably be utf8.toString,
-- however i don't know if encoding is consistant.
-- it should probably be overridable.
pathDecode :: B.ByteString -> FilePath
pathDecode = BC.unpack

pathEncode :: FilePath -> B.ByteString
pathEncode = BC.pack

toRefTy :: B.ByteString -> RefTy
toRefTy s
    | "refs/tags/" `BC.isPrefixOf` s    = RefTyTag $ pathDecode $ B.drop 10 s
    | "refs/heads/" `BC.isPrefixOf` s   = RefTyHead $ pathDecode $ B.drop 11 s
    | "refs/remotes/" `BC.isPrefixOf` s = RefTyRemote $ pathDecode $ B.drop 13 s
    | "refs/patches/" `BC.isPrefixOf` s = RefTyPatches $ pathDecode $ B.drop 13 s
    | "refs/stash" == s                 = RefTyStash
    | otherwise                         = RefTyOther $ pathDecode $ s

fromRefTy :: RefTy -> B.ByteString
fromRefTy (RefTyHead h)    = "refs/heads/" `B.append` pathEncode h
fromRefTy (RefTyTag h)     = "refs/tags/" `B.append` pathEncode h
fromRefTy (RefTyRemote h)  = "refs/remotes/" `B.append` pathEncode h
fromRefTy (RefTyPatches h) = "refs/patches/" `B.append` pathEncode h
fromRefTy RefTyStash       = "refs/stash"
fromRefTy (RefTyOther h)   = pathEncode h

getDirectoryContentNoDots path = filter noDot <$> getDirectoryContents path
        where noDot = (not . isPrefixOf ".")

readPackedRefs gitRepo = foldl accu [] . BC.lines <$> B.readFile (packedRefsPath gitRepo)
    where accu a l
            | "#" `BC.isPrefixOf` l = a
            | otherwise = let (ref, r) = B.splitAt 40 l
                              name     = B.tail r
                           in (fromHex ref, toRefTy name) : a

headsList gitRepo = getDirectoryContentNoDots (headsPath gitRepo)
tagsList gitRepo = getDirectoryContentNoDots (tagsPath gitRepo)
remotesList gitRepo = getDirectoryContentNoDots (remotesPath gitRepo)
remoteList gitRepo remote = getDirectoryContentNoDots (remotePath gitRepo remote)

writeRef path ref = B.writeFile path (B.concat [toHex ref, B.singleton 0xa])
readRef path = fromHex . B.take 40 <$> B.readFile path

readRefAndFollow gitRepo path = do
        content <- B.readFile path
        if "ref: " `B.isPrefixOf` content
                then do -- the whole thing is really fragile. need to do the proper thing.
                        let file = pathDecode $ BC.init $ B.drop 5 content
                        readRefAndFollow gitRepo (gitRepo ++ "/" ++ file)
                else return (fromHex $ B.take 40 content)

headExists gitRepo name    = doesFileExist (headPath gitRepo name)
headRead gitRepo name      = readRef (headPath gitRepo name)
headWrite gitRepo name ref = writeRef (headPath gitRepo name) ref

tagExists gitRepo name    = doesFileExist (tagPath gitRepo name)
tagRead gitRepo name      = readRef (tagPath gitRepo name)
tagWrite gitRepo name ref = writeRef (tagPath gitRepo name) ref

specialRead gitRepo name   = readRefAndFollow gitRepo (specialPath gitRepo name)
specialExists gitRepo name = doesFileExist (specialPath gitRepo name)
