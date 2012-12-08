-- |
-- Module      : Data.Git.Revision
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
module Data.Git.Revision
        ( Revision(..)
        , RevModifier(..)
        , fromString
        ) where

import Text.Parsec
import Data.String

data RevModifier =
          RevModParent Int       -- ^ parent accessor ^<n> and ^
        | RevModParentFirstN Int -- ^ parent accessor ~<n>
        | RevModAtType String    -- ^ @{type} accessor
        | RevModAtDate String    -- ^ @{date} accessor
        | RevModAtN Int          -- ^ @{n} accessor
        deriving (Eq)

instance Show RevModifier where
    show (RevModParent 1)       = "^"
    show (RevModParent n)       = "^" ++ show n
    show (RevModParentFirstN n) = "~" ++ show n
    show (RevModAtType s)       = "@{" ++ s ++ "}"
    show (RevModAtDate s)       = "@{" ++ s ++ "}"
    show (RevModAtN s)          = "@{" ++ show s ++ "}"

data Revision = Revision String [RevModifier]
        deriving (Eq)

instance Show Revision where
    show (Revision s ms) = s ++ concatMap show ms

instance IsString Revision where
    fromString = revFromString

revFromString s = either (error.show) id $ parse parser "" s where
        parser = do
                p    <- many (noneOf "^~@")
                mods <- many (choice [parseParent, parseFirstParent, parseAt])
                return $ Revision p mods
        parseParent = try $ do
                _ <- char '^'
                n <- optionMaybe (many1 digit)
                case n of
                        Nothing -> return $ RevModParent 1
                        Just d  -> return $ RevModParent (read d)
        parseFirstParent = try $
                char '~' >> many1 digit >>= return . RevModParentFirstN . read
        parseAt = try $ do
                _  <- char '@' >> char '{'
                at <- choice [ parseAtType, parseAtDate, parseAtN ]
                _  <- char '}'
                return at
        parseAtType = try $ do
                ty <- choice $ map string ["tree","commit","blob","tag"]
                return $ RevModAtType ty
        parseAtN = try $ do
                many1 digit >>= return . RevModAtN . read
        parseAtDate = try $ do
                many (noneOf "}") >>= return . RevModAtDate
