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
    , Tree(..)
    , Blob(..)
    , Tag(..)

    -- * repo context
    , withCurrentRepo
    , withRepo
    , findRepo

    -- * Get objects
    , getObject
    , getCommit
    , getTree
    ) where

import Data.Git.Ref
import Data.Git.Types
import Data.Git.Storage
import Data.Git.Repository
