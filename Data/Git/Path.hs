-- |
-- Module      : Data.Git.Path
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
module Data.Git.Path where

import System.FilePath
import System.Random
import Control.Applicative ((<$>))
import Data.Git.Ref

headsPath gitRepo = gitRepo </> "refs" </> "heads"
tagsPath gitRepo  = gitRepo </> "refs" </> "tags"
remotesPath gitRepo = gitRepo </> "refs" </> "remotes"

headPath gitRepo name = headsPath gitRepo </> name
tagPath gitRepo name = tagsPath gitRepo </> name
remotePath gitRepo name = remotesPath gitRepo </> name
specialPath gitRepo name = gitRepo </> name

remoteEntPath gitRepo name ent = remotePath gitRepo name </> ent

packDirPath repoPath = repoPath </> "objects" </> "pack"

indexPath repoPath indexRef =
	packDirPath repoPath </> ("pack-" ++ toHexString indexRef ++ ".idx")

packPath repoPath packRef =
	packDirPath repoPath </> ("pack-" ++ toHexString packRef ++ ".pack")

objectPath repoPath d f = repoPath </> "objects" </> d </> f
objectPathOfRef repoPath ref = objectPath repoPath d f
	where (d,f) = toFilePathParts ref

objectTemporaryPath repoPath = do
	r <- fst . random <$> getStdGen :: IO Int
	return (repoPath </> "objects" </> ("tmp-" ++ show r ++ ".tmp"))
