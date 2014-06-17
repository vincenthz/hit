{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Data.Git.Storage
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--

module Data.Git.Storage
    ( Git
    , GitStorage(..)
    , gitRepoPath
    , FileBackend
    , gitFilePath
    , gitBackend
    -- * opening repositories
    , openRepo
    , closeRepo
    , withRepo
    , withCurrentRepo
    , findRepoMaybe
    , findRepo
    , isRepo
    -- * creating repositories
    , initRepo
    -- * repository accessors
    , getDescription
    , setDescription
    -- * getting objects
    , getObjectRaw
    , getObject
    , getObject_
    , getObjectType
    -- * setting objects
    , setObject
    ) where

import Filesystem
import Filesystem.Path hiding (concat)
import Filesystem.Path.Rules
import System.Environment

import Control.Applicative
import Control.Exception
import qualified Control.Exception as E
import Control.Monad

import Data.String

import Data.Git.Storage.Object
import Data.Git.Storage.Pack
import Data.Git.Storage.Backend
import Data.Git.Storage.FileBackend
import Data.Git.Ref

import Prelude hiding (FilePath)

-- | represent a git repo, with possibly already opened filereaders
-- for indexes and packs
data Git = Git
    { gitBackend :: FileBackend
    }

gitRepoPath = gitFilePath . gitBackend

instance GitStorage Git where
    closeStorage git = closeStorage (gitBackend git)

    objectGetRaw git = objectGetRaw (gitBackend git)
    objectSet git    = objectSet (gitBackend git)
    objectType git   = objectType (gitBackend git)
    objectFindPrefix git = objectFindPrefix (gitBackend git)

    -- description
    descriptionGet git = descriptionGet (gitBackend git)
    descriptionSet git = descriptionSet (gitBackend git)

    configGet git = configGet (gitBackend git)

    refRead git = refRead (gitBackend git)
    branchList git = branchList (gitBackend git)
    branchWrite git = branchWrite (gitBackend git)
    tagList git = tagList (gitBackend git)
    tagWrite git = tagWrite (gitBackend git)

    headGet git = headGet (gitBackend git)
    headSet git = headSet (gitBackend git)

-- | open a new git repository context
openRepo :: FilePath -> IO Git
openRepo path = Git <$> newFileBackend path

-- | close a git repository context, closing all remaining fileReaders.
closeRepo :: Git -> IO ()
closeRepo git = closeStorage git

-- | Find the git repository from the current directory.
--
-- If the environment variable GIT_DIR is set then it's used,
-- otherwise iterate from current directory, up to 128 parents for a .git directory
findRepoMaybe :: IO (Maybe FilePath)
findRepoMaybe = do
    menvDir <- E.catch (Just . decodeString posix_ghc704 <$> getEnv "GIT_DIR") (\(_:: SomeException) -> return Nothing)
    case menvDir of
        Nothing     -> getWorkingDirectory >>= checkDir 0
        Just envDir -> isRepo envDir >>= \e -> return (if e then Just envDir else Nothing)
  where checkDir :: Int -> FilePath -> IO (Maybe FilePath)
        checkDir 128 _  = return Nothing
        checkDir n   wd = do
            let filepath = wd </> ".git"
            e <- isRepo filepath
            if e then return (Just filepath) else checkDir (n+1) (if absolute wd then parent wd else wd </> "..")

-- | Find the git repository from the current directory.
--
-- If the environment variable GIT_DIR is set then it's used,
-- otherwise iterate from current directory, up to 128 parents for a .git directory
findRepo :: IO FilePath
findRepo = do
    menvDir <- E.catch (Just . decodeString posix_ghc704 <$> getEnv "GIT_DIR") (\(_:: SomeException) -> return Nothing)
    case menvDir of
        Nothing     -> getWorkingDirectory >>= checkDir 0
        Just envDir -> do
            e <- isRepo envDir
            when (not e) $ error "environment GIT_DIR is not a git repository" 
            return envDir
  where checkDir :: Int -> FilePath -> IO FilePath
        checkDir 128 _  = error "not a git repository"
        checkDir n   wd = do
            let filepath = wd </> ".git"
            e <- isRepo filepath
            if e then return filepath else checkDir (n+1) (if absolute wd then parent wd else wd </> "..")

-- | execute a function f with a git context.
withRepo path f = bracket (openRepo path) closeRepo f

-- | execute a function on the current repository.
--
-- check findRepo to see how the git repository is found.
withCurrentRepo :: (Git -> IO a) -> IO a
withCurrentRepo f = findRepo >>= \path -> withRepo path f

-- | basic checks to see if a specific path looks like a git repo.
isRepo :: FilePath -> IO Bool
isRepo path = do
    dir     <- isDirectory path
    subDirs <- mapM (isDirectory . (path </>))
                    [ "hooks", "info"
                    , "logs", "objects", "refs"
                    , "refs"</> "heads", "refs"</> "tags"]
    return $ and ([dir] ++ subDirs)

-- | initialize a new repository at a specific location.
initRepo :: FilePath -> IO ()
initRepo path = do
    exists <- isDirectory path
    when exists $ error "destination directory already exists"
    createDirectory True path
    mapM_ (createDirectory False . (path </>))
        [ "branches", "hooks", "info"
        , "logs", "objects", "refs"
        , "refs"</> "heads", "refs"</> "tags"]

-- | get an object from repository
getObjectRaw :: Git -> Ref -> Bool -> IO (Maybe ObjectInfo)
getObjectRaw git ref resolveDelta = objectGetRaw git ref resolveDelta

-- | get an object from repository using a ref.
getObject :: Git               -- ^ repository
          -> Ref               -- ^ the object's reference to
          -> Bool              -- ^ whether to resolve deltas if found
          -> IO (Maybe Object) -- ^ returned object if found
getObject git ref resolveDelta = maybe Nothing toObj <$> getObjectRaw git ref resolveDelta
  where toObj (ObjectInfo { oiHeader = (ty, _, extra), oiData = objData }) = packObjectFromRaw (ty, extra, objData)

-- | Just like 'getObject' but will raise a RefNotFound exception if the
-- reference cannot be found.
getObject_ :: Git       -- ^ repository
           -> Ref       -- ^ the object's reference to
           -> Bool      -- ^ whether to resolve deltas if found
           -> IO Object -- ^ returned object if found
getObject_ git ref resolveDelta = maybe (throwIO $ RefNotFound ref) return
                              =<< getObject git ref resolveDelta

-- | set an object in the store and returns the new ref
-- this is always going to create a loose object.
setObject :: Git
          -> Object
          -> IO Ref
setObject git obj = objectSet git obj


getDescription :: Git -> IO (Maybe String)
getDescription = descriptionGet

setDescription :: Git -> String -> IO ()
setDescription = descriptionSet

getObjectType :: Git -> Ref -> IO (Maybe ObjectType)
getObjectType = objectType
