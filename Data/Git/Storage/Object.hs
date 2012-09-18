{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Data.Git.Storage.Object
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
module Data.Git.Storage.Object
        ( ObjectLocation(..)
        , ObjectType(..)
        , ObjectHeader
        , ObjectData
        , ObjectPtr(..)
        , Object(..)
        , ObjectInfo(..)
        , Objectable(..)
        , objectToType
        , objectTypeMarshall
        , objectTypeUnmarshall
        , objectTypeIsDelta
        , objectIsDelta
        , objectToTree
        , objectToCommit
        , objectToTag
        , objectToBlob
        -- * parsing function
        , treeParse
        , commitParse
        , tagParse
        , blobParse
        , objectParseTree
        , objectParseCommit
        , objectParseTag
        , objectParseBlob
        -- * writing function
        , objectWriteHeader
        , objectWrite
        , objectHash
        ) where

import Data.Git.Ref
import Data.Git.Types

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L

import Data.Attoparsec.Lazy
import qualified Data.Attoparsec.Lazy as P
import qualified Data.Attoparsec.Char8 as PC
import Control.Applicative ((<$>), (<*), (*>), many)
import Control.Monad

import Data.List (intersperse)
import Data.Monoid
import Data.Word
import Text.Printf

import Data.Time.LocalTime
import Data.Time.Clock.POSIX

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8

-- | location of an object in the database
data ObjectLocation = NotFound | Loose Ref | Packed Ref Word64
        deriving (Show,Eq)

-- | Delta objects points to some others objects in the database
-- either as offset in the pack or as a direct reference.
data ObjectPtr = PtrRef Ref | PtrOfs Word64 deriving (Show,Eq)

type ObjectHeader = (ObjectType, Word64, Maybe ObjectPtr)

type ObjectData = L.ByteString

-- | Raw objects infos have an header (type, size, ptr),
-- the data and a pointers chains to parents for resolved objects.
data ObjectInfo = ObjectInfo
        { oiHeader :: ObjectHeader
        , oiData   :: ObjectData
        , oiChains :: [ObjectPtr]
        } deriving (Show,Eq)

-- | describe a git object, that could of 6 differents types:
-- tree, blob, commit, tag and deltas (offset or ref).
-- the deltas one are only available in packs.
data Object = ObjCommit   Commit
            | ObjTag      Tag
            | ObjBlob     Blob
            | ObjTree     Tree
            | ObjDeltaOfs DeltaOfs
            | ObjDeltaRef DeltaRef
            deriving (Show,Eq)

class Objectable a where
        getType  :: a -> ObjectType
        getRaw   :: a -> L.ByteString
        isDelta  :: a -> Bool
        toObject :: a -> Object

objectToType :: Object -> ObjectType
objectToType (ObjTree _)     = TypeTree
objectToType (ObjBlob _)     = TypeBlob
objectToType (ObjCommit _)   = TypeCommit
objectToType (ObjTag _)      = TypeTag
objectToType (ObjDeltaOfs _) = TypeDeltaOff
objectToType (ObjDeltaRef _) = TypeDeltaRef

objectTypeMarshall :: ObjectType -> String
objectTypeMarshall TypeTree   = "tree"
objectTypeMarshall TypeBlob   = "blob"
objectTypeMarshall TypeCommit = "commit"
objectTypeMarshall TypeTag    = "tag"
objectTypeMarshall _          = error "deltas cannot be marshalled"

objectTypeUnmarshall :: String -> ObjectType
objectTypeUnmarshall "tree"   = TypeTree
objectTypeUnmarshall "blob"   = TypeBlob
objectTypeUnmarshall "commit" = TypeCommit
objectTypeUnmarshall "tag"    = TypeTag
objectTypeUnmarshall _        = error "unknown object type"

objectTypeIsDelta :: ObjectType -> Bool
objectTypeIsDelta TypeDeltaOff = True
objectTypeIsDelta TypeDeltaRef = True
objectTypeIsDelta _            = False

objectIsDelta :: Object -> Bool
objectIsDelta (ObjDeltaOfs _) = True
objectIsDelta (ObjDeltaRef _) = True
objectIsDelta _               = False

objectToTree :: Object -> Maybe Tree
objectToTree (ObjTree tree) = Just tree
objectToTree _              = Nothing

objectToCommit :: Object -> Maybe Commit
objectToCommit (ObjCommit commit) = Just commit
objectToCommit _                  = Nothing

objectToTag :: Object -> Maybe Tag
objectToTag (ObjTag tag) = Just tag
objectToTag _            = Nothing

objectToBlob :: Object -> Maybe Blob
objectToBlob (ObjBlob blob) = Just blob
objectToBlob _              = Nothing

octal :: Parser Int
octal = B.foldl' step 0 `fmap` takeWhile1 isOct where
        isOct w = w >= 0x30 && w <= 0x37
        step a w = a * 8 + fromIntegral (w - 0x30)

tillEOL :: Parser ByteString
tillEOL = PC.takeWhile ((/= '\n'))

skipEOL = skipChar '\n'

skipChar :: Char -> Parser ()
skipChar c = PC.char c >> return ()

referenceHex = fromHex <$> P.take 40
referenceBin = fromBinary <$> P.take 20

-- | parse a tree content
treeParse = (Tree <$> parseEnts) where
        parseEnts = atEnd >>= \end -> if end then return [] else liftM2 (:) parseEnt parseEnts
        parseEnt = liftM3 (,,) octal (PC.char ' ' >> takeTill ((==) 0)) (word8 0 >> referenceBin)
-- | parse a blob content
blobParse = (Blob <$> takeLazyByteString)

-- | parse a commit content
commitParse = do
        tree <- string "tree " >> referenceHex
        skipChar '\n'
        parents   <- many parseParentRef
        author    <- string "author " >> parseName
        committer <- string "committer " >> parseName
        encoding  <- option Nothing $ Just <$> (string "encoding " >> tillEOL)
        extras    <- many parseExtra
        skipChar '\n'
        message <- takeByteString
        return $ Commit tree parents author committer encoding extras message
        where
                parseParentRef = do
                        tree <- string "parent " >> referenceHex
                        skipChar '\n'
                        return tree
                parseExtra = do
                        f <- B.pack . (:[]) <$> notWord8 0xa
                        r <- tillEOL
                        skipEOL
                        v <- concatLines <$> many (string " " *> tillEOL <* skipEOL)
                        return $ CommitExtra (f `B.append` r) v
                concatLines = B.concat . intersperse (B.pack [0xa])

-- | parse a tag content
tagParse = do
        object <- string "object " >> referenceHex
        skipChar '\n'
        type_ <- objectTypeUnmarshall . BC.unpack <$> (string "type " >> takeTill ((==) 0x0a))
        skipChar '\n'
        tag   <- string "tag " >> takeTill ((==) 0x0a)
        skipChar '\n'
        tagger <- string "tagger " >> parseName
        skipChar '\n'
        signature <- takeByteString
        return $ Tag object type_ tag tagger signature

parseName = do
        name <- B.init <$> PC.takeWhile ((/=) '<')
        skipChar '<'
        email <- PC.takeWhile ((/=) '>')
        _ <- string "> "
        time <- PC.decimal :: Parser Integer
        _ <- string " "
        timezone <- PC.signed PC.decimal
        let (hours,minutes) = timezone `divMod` 100
        skipChar '\n'
        let tz = minutesToTimeZone (hours*60 + (if hours > 0 then minutes else (-minutes)))
        return (name, email, posixSecondsToUTCTime $ fromIntegral time, tz)

objectParseTree   = ObjTree <$> treeParse
objectParseCommit = ObjCommit <$> commitParse
objectParseTag    = ObjTag <$> tagParse
objectParseBlob   = ObjBlob <$> blobParse

-- header of loose objects, but also useful for any object to determine object's hash
objectWriteHeader :: ObjectType -> Word64 -> ByteString
objectWriteHeader ty sz = BC.pack (objectTypeMarshall ty ++ " " ++ show sz ++ [ '\0' ])

objectWrite :: Object -> L.ByteString
objectWrite (ObjCommit commit) = commitWrite commit
objectWrite (ObjTag tag)       = tagWrite tag
objectWrite (ObjBlob blob)     = blobWrite blob
objectWrite (ObjTree tree)     = treeWrite tree
objectWrite _                  = error "delta cannot be marshalled"

treeWrite (Tree ents) = L.fromChunks $ concat $ map writeTreeEnt ents where
        writeTreeEnt (perm,name,ref) =
                [ BC.pack $ printf "%o" perm
                , BC.singleton ' '
                , name
                , B.singleton 0
                , toBinary ref
                ]

commitWrite (Commit tree parents author committer encoding extra msg) =
    toLazyByteString $ mconcat els
    where
          toNamedRef s r = mconcat [fromString s, fromByteString (toHex r),eol]
          toParent       = toNamedRef "parent "
          toCommitExtra :: CommitExtra -> [Builder]
          toCommitExtra (CommitExtra k v) = [fromByteString k, eol] ++
                                            (concatMap (\l -> [fromByteString " ", fromByteString l, eol]) $ linesLast v)

          linesLast b
            | B.length b > 0 && B.last b == 0xa = BC.lines b ++ [ "" ]
            | otherwise                         = BC.lines b
          els = [toNamedRef "tree " tree ]
             ++ map toParent parents
             ++ [fromByteString $ writeName "author" author, eol
                ,fromByteString $ writeName "committer" committer, eol
                ,maybe (fromByteString B.empty) (fromByteString) encoding -- FIXME need eol
                ]
             ++ concatMap toCommitExtra extra
             ++ [eol
                ,fromByteString msg
                ]

tagWrite (Tag ref ty tag tagger signature) =
        L.fromChunks [BC.unlines ls, B.singleton 0xa, signature]
        where
                ls = [ "object " `BC.append` (toHex ref)
                     , "type " `BC.append` (BC.pack $ objectTypeMarshall ty)
                     , "tag " `BC.append` tag
                     , writeName "tagger" tagger ]

eol = fromString "\n"

blobWrite (Blob bData) = bData

instance Objectable Blob where
        getType _ = TypeBlob
        getRaw    = blobWrite
        toObject  = ObjBlob
        isDelta   = const False

instance Objectable Commit where
        getType _ = TypeCommit
        getRaw    = commitWrite
        toObject  = ObjCommit
        isDelta   = const False

instance Objectable Tag where
        getType _ = TypeTag
        getRaw    = tagWrite
        toObject  = ObjTag
        isDelta   = const False

instance Objectable Tree where
        getType _ = TypeTree
        getRaw    = treeWrite
        toObject  = ObjTree
        isDelta   = const False

instance Objectable DeltaOfs where
        getType _ = TypeDeltaOff
        getRaw    = error "delta offset cannot be marshalled"
        toObject  = ObjDeltaOfs
        isDelta   = const True

instance Objectable DeltaRef where
        getType _ = TypeDeltaRef
        getRaw    = error "delta ref cannot be marshalled"
        toObject  = ObjDeltaRef
        isDelta   = const True

objectHash :: ObjectType -> Word64 -> L.ByteString -> Ref
objectHash ty w lbs = hashLBS $ L.fromChunks (objectWriteHeader ty w : L.toChunks lbs)

-- used for objectWrite for commit and tag
writeName label (name, email, time, tz) =
        B.concat [label, " ", name, " <", email, "> ", BC.pack (printf "%d %s" timeSeconds (timeZoneOffsetString tz)) ]
        where timeSeconds :: Integer
              timeSeconds = truncate (realToFrac $ utcTimeToPOSIXSeconds time :: Double)
