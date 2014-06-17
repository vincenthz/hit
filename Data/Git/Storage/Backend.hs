-- |
-- Module      : Data.Git.Storage.Backend
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
-- Generic storage backend for git
module Data.Git.Storage.Backend
    ( GitStorage(..)
    ) where

import Data.Git.Ref
import Data.Git.Named
import Data.Git.Config
import Data.Git.Storage.Object
import Data.Set (Set)

-- | Provide all the methods that are needed to
-- store and retrieve objects and references to them.
class GitStorage a where
    -- | Cleanly close this git storage instance
    closeStorage :: a -> IO ()

    -- | get a raw object
    objectGetRaw :: a -> Ref -> Bool -> IO (Maybe ObjectInfo)

    -- | Create a new object in the store
    objectSet    :: a -> Object -> IO Ref

    -- | Get the type of a specific object if it exists
    objectType   :: a -> Ref -> IO (Maybe ObjectType)

    -- | Find reference that starts by a specific prefix
    objectFindPrefix :: a -> String -> IO [Ref]

    -- | Get the description
    descriptionGet :: a -> IO (Maybe String)

    -- | Set the description
    descriptionSet :: a -> String -> IO ()

    -- | Get the Configuration
    configGet :: a -> IO Config

    -- | Read a named reference
    refRead :: a -> RefSpecTy -> IO (Maybe Ref)

    -- | Return the list of branches
    branchList :: a -> IO (Set RefName)

    -- | Write a branch to point to a specific reference
    branchWrite :: a       -- ^ storage backend
                -> RefName -- ^ the name of the branch to write
                -> Ref     -- ^ the reference to set
                -> IO ()

    -- | Return the list of tag
    tagList :: a -> IO (Set RefName)

    -- | Write a tag to point to a specific reference
    tagWrite :: a
             -> RefName -- ^ the name of the tag to write
             -> Ref     -- ^ the reference to set
             -> IO ()

    -- | Get what the head is pointing to, or the reference otherwise
    headGet :: a -> IO (Either Ref RefName)

    -- | Set head to point to either a reference or a branch name.
    headSet :: a                  -- ^ storage backend
            -> Either Ref RefName -- ^ either a raw reference or a branch name
            -> IO ()
