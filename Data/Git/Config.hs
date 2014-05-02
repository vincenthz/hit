-- |
-- Module      : Data.Git.Config
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
-- config related types and methods.
--
module Data.Git.Config
    ( Config(..)
    , Section(..)
    -- * methods
    , readConfig
    ) where

import Control.Applicative
import Data.Git.Path
import Filesystem.Path.CurrentOS

newtype Config = Config [Section]
    deriving (Show,Eq)

data Section = Section
    { sectionName :: String
    , sectionKVs  :: [(String, String)]
    } deriving (Show,Eq)

parseConfig :: String -> Config
parseConfig = Config . reverse . toSections . foldl accSections ([], Nothing) . lines
  where toSections (l,Nothing) = l
        toSections (l,Just s)  = s : l

        -- a new section in the config file
        accSections (sections, mcurrent) ('[':sectNameE)
            | last sectNameE == ']' =
                let sectName = take (length sectNameE - 1) sectNameE
                 in case mcurrent of
                    Nothing      -> (sections, Just $ Section sectName [])
                    Just current -> (sectionFinalize current : sections, Just $ Section sectName [])
            | otherwise             =
                (sections, mcurrent)
        -- a normal line without having any section defined yet
        accSections acc@(_, Nothing) _ = acc
        -- potentially a k-v line in an existing section
        accSections (sections, Just current) kvLine =
            case break (== '=') kvLine of
                (k,'=':v) -> (sections, Just $ sectionAppend current (strip k, strip v))
                (_,_)     -> (sections, Just current) -- not a k = v line
        -- append a key-value
        sectionAppend (Section n l) kv = Section n (kv:l)
        sectionFinalize (Section n l) = Section n $ reverse l

        strip s = dropSpaces $ reverse $ dropSpaces $ reverse s
          where dropSpaces = dropWhile (\c -> c == ' ' || c == '\t')

readConfigPath filepath = parseConfig <$> readFile (encodeString filepath)
readConfig gitRepo = readConfigPath (configPath gitRepo)
