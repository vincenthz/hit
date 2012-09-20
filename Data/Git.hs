-- |
-- Module      : Data.Git
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
module Data.Git
    (
    -- * Basic types
      Ref
    , Commit(..)
    , Person(..)
    , CommitExtra(..)
    , Tree(..)
    , Blob(..)
    , Tag(..)
    , GitTime(..)

    -- * Revision
    , Revision
    , resolveRevision

    -- * Object resolution
    , resolveTreeish
    , resolvePath

    -- * repo context
    , withCurrentRepo
    , withRepo
    , findRepo

    -- * Repository queries and creation
    , initRepo
    , isRepo

    -- * Context operations
    , rewrite

    -- * Get objects
    , getObject
    , getCommit
    , getTree

    -- * Set objects
    , setObject
    ) where

import Data.Git.Ref
import Data.Git.Types
import Data.Git.Storage
import Data.Git.Repository
import Data.Git.Revision
