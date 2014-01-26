-- |
-- Module      : Data.Git.Object
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Git.Types
    (
    -- * Type of types
      ObjectType(..)
    -- * Main git types
    , Tree(..)
    , Commit(..)
    , CommitExtra(..)
    , Blob(..)
    , Tag(..)
    , Person(..)
    -- * time type
    , GitTime(..)
    , toUTCTime
    , toPOSIXTime
    , toZonedTime
    -- * Pack delta types
    , DeltaOfs(..)
    , DeltaRef(..)
    -- * Basic types part of other bigger types
    , TreeEnt
    ) where

import Data.Word
import Data.Monoid
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L

import Data.Git.Ref
import Data.Git.Delta
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Clock.POSIX
import Data.Data

-- | type of a git object.
data ObjectType =
      TypeTree
    | TypeBlob
    | TypeCommit
    | TypeTag
    | TypeDeltaOff
    | TypeDeltaRef
    deriving (Show,Eq,Data,Typeable)

-- | Git time is number of seconds since unix epoch with a timezone
data GitTime = GitTime Integer Int
    deriving (Show,Eq)

toUTCTime :: GitTime -> UTCTime
toUTCTime = zonedTimeToUTC . toZonedTime

toPOSIXTime :: GitTime -> POSIXTime
toPOSIXTime = utcTimeToPOSIXSeconds . toUTCTime

toZonedTime :: GitTime -> ZonedTime
toZonedTime (GitTime epoch tzHourMin) = zonedTime
  where tz        = minutesToTimeZone (signum h * minutes)
        (h,m)     = tzHourMin `divMod` 100
        minutes   = abs h * 60 + m
        utcTime   = posixSecondsToUTCTime $ fromIntegral epoch
        zonedTime = utcToZonedTime tz utcTime

-- | the enum instance is useful when marshalling to pack file.
instance Enum ObjectType where
    fromEnum TypeCommit   = 0x1
    fromEnum TypeTree     = 0x2
    fromEnum TypeBlob     = 0x3
    fromEnum TypeTag      = 0x4
    fromEnum TypeDeltaOff = 0x6
    fromEnum TypeDeltaRef = 0x7

    toEnum 0x1 = TypeCommit
    toEnum 0x2 = TypeTree
    toEnum 0x3 = TypeBlob
    toEnum 0x4 = TypeTag
    toEnum 0x6 = TypeDeltaOff
    toEnum 0x7 = TypeDeltaRef
    toEnum n   = error ("not a valid object: " ++ show n)

-- | represent one entry in the tree
-- (permission,file or directory name,blob or tree ref)
-- name should maybe a filepath, but not sure about the encoding.
type TreeEnt = (Int,ByteString,Ref)

-- | an author or committer line
-- has the format: name <email> time timezone
-- FIXME: should be a string, but I don't know if the data is stored
-- consistantly in one encoding (UTF8)
data Person = Person
    { personName  :: ByteString
    , personEmail :: ByteString
    , personTime  :: GitTime
    } deriving (Show,Eq)

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
    , commitAuthor    :: Person
    , commitCommitter :: Person
    , commitEncoding  :: Maybe ByteString
    , commitExtras    :: [CommitExtra]
    , commitMessage   :: ByteString
    } deriving (Show,Eq)

data CommitExtra = CommitExtra
    { commitExtraKey   :: ByteString
    , commitExtraValue :: ByteString
    } deriving (Show,Eq)

-- | Represent a signed tag.
data Tag = Tag
    { tagRef        :: Ref
    , tagObjectType :: ObjectType
    , tagBlob       :: ByteString
    , tagName       :: Person
    , tagS          :: ByteString
    } deriving (Show,Eq)

-- | Delta pointing to an offset.
data DeltaOfs = DeltaOfs Word64 Delta
    deriving (Show,Eq)

-- | Delta pointing to a ref.
data DeltaRef = DeltaRef Ref Delta
    deriving (Show,Eq)
