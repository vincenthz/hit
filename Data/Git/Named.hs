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

getDirectoryContentNoDots path = filter noDot <$> getDirectoryContents path
        where noDot = (not . isPrefixOf ".")

headsList gitRepo = getDirectoryContentNoDots (headsPath gitRepo)
tagsList gitRepo = getDirectoryContentNoDots (tagsPath gitRepo)
remotesList gitRepo = getDirectoryContentNoDots (remotesPath gitRepo)
remoteList gitRepo remote = getDirectoryContentNoDots (remotePath gitRepo remote)

writeRef path ref = B.writeFile path (B.concat [toHex ref, B.singleton 0xa])
readRef path = fromHex . B.take 40 <$> B.readFile path

readRefAndFollow gitRepo path = do
        content <- B.readFile path
        if "ref: " `B.isPrefixOf` content
                then do -- BC.unpack should be utf8.toString, and the whole thing is really fragile. need to do the proper thing.
                        let file = BC.unpack $ BC.init $ B.drop 5 content
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
