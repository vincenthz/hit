-- |
-- Module      : Data.Git.Object
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
module Data.Git.Types
    (
    -- * Type of types
      ObjectType(..)
    -- * Main git types
    , Tree(..)
    , Commit(..)
    , Blob(..)
    , Tag(..)
    -- * Pack delta types
    , DeltaOfs(..)
    , DeltaRef(..)
    -- * Basic types part of other bigger types
    , TreeEnt
    , Name
    ) where

import Data.Word
import Data.Monoid
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L

import Data.Git.Ref
import Data.Git.Delta

-- | type of a git object.
data ObjectType =
          TypeTree
        | TypeBlob
        | TypeCommit
        | TypeTag
        | TypeDeltaOff
        | TypeDeltaRef
        deriving (Show,Eq)

-- | represent one entry in the tree
-- (permission,file or directory name,blob or tree ref)
-- name should maybe a filepath, but not sure about the encoding.
type TreeEnt = (Int,ByteString,Ref)

-- | an author or committer line
-- has the format: name <email> time timezone
-- FIXME: should be a string, but I don't know if the data is stored
-- consistantly in one encoding (UTF8)
type Name = (ByteString,ByteString,Int,Int)

-- | Represent a root tree with zero to many tree entries.
data Tree = Tree { treeGetEnts :: [TreeEnt] } deriving (Show,Eq)

instance Monoid Tree where
    mempty                      = Tree []
    mappend (Tree e1) (Tree e2) = Tree (e1 ++ e2)
    mconcat trees               = Tree $ concatMap treeGetEnts trees

-- | Represent a binary blob.
data Blob = Blob { blobGetContent :: L.ByteString } deriving (Show,Eq)

-- | Represent a commit object.
data Commit = Commit
        { commitTreeish   :: Ref
        , commitParents   :: [Ref]
        , commitAuthor    :: Name
        , commitCommitter :: Name
        , commitMessage   :: ByteString
        } deriving (Show,Eq)

-- | Represent a signed tag.
data Tag = Tag
        { tagRef        :: Ref
        , tagObjectType :: ObjectType
        , tagBlob       :: ByteString
        , tagName       :: Name
        , tagS          :: ByteString
        } deriving (Show,Eq)

-- | Delta pointing to an offset.
data DeltaOfs = DeltaOfs Word64 Delta
        deriving (Show,Eq)

-- | Delta pointing to a ref.
data DeltaRef = DeltaRef Ref Delta
        deriving (Show,Eq)
