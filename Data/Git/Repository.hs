{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      : Data.Git.Repository
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
module Data.Git.Repository
        ( Git
        , HTree
        , HTreeEnt(..)
        , getCommit
        , getTree
        , rewrite
        , buildHTree
        , resolvePath
        , resolveTreeish
        , resolveRevision
        , initRepo
        , isRepo
        ) where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Exception (Exception)

import Data.Maybe (fromMaybe)
import Data.List (find)
import Data.Data

import Data.ByteString (ByteString)

import Data.Git.Named
import Data.Git.Types
import Data.Git.Storage.Object
import Data.Git.Storage
import Data.Git.Revision
import Data.Git.Storage.Loose
import Data.Git.Storage.CacheFile
import Data.Git.Ref

import qualified Data.Map as M

-- | hierarchy tree, either a reference to a blob (file) or a tree (directory).
data HTreeEnt = TreeDir Ref HTree | TreeFile Ref
type HTree = [(Int,ByteString,HTreeEnt)]

-- | Exception when trying to convert an object pointed by 'Ref' to
-- a type that is different
data InvalidType = InvalidType Ref ObjectType
                 deriving (Show,Eq,Data,Typeable)

instance Exception InvalidType

-- should be a standard function that do that...
mapJustM f (Just o) = f o
mapJustM _ Nothing  = return Nothing

-- | get a specified commit
getCommit :: Git -> Ref -> IO (Maybe Commit)
getCommit git ref = maybe Nothing objectToCommit <$> getObject git ref True

-- | get a specified tree
getTree :: Git -> Ref -> IO (Maybe Tree)
getTree git ref = maybe Nothing objectToTree <$> getObject git ref True

-- | try to resolve a string to a specific commit ref
-- for example: HEAD, HEAD^, master~3, shortRef
resolveRevision :: Git -> Revision -> IO (Maybe Ref)
resolveRevision git (Revision prefix modifiers) =
    getCacheVal (packedNamed git) >>= \c -> resolvePrefix c >>= modf modifiers
    where
          resolvePrefix lookupCache = tryResolvers
                [resolveNamedPrefix lookupCache namedResolvers
                ,resolvePrePrefix
                ]

          resolveNamedPrefix _           []     = return Nothing
          resolveNamedPrefix lookupCache (x:xs) = followToRef (resolveNamedPrefix lookupCache xs) x
            where followToRef onFailure refty = do
                      exists <- existsRefFile (gitRepoPath git) refty
                      if exists
                          then do refcont <- readRefFile (gitRepoPath git) refty
                                  case refcont of
                                       RefDirect ref     -> return $ Just ref
                                       RefLink refspecty -> followToRef onFailure refspecty
                                       _                 -> error "cannot handle reference content"
                          else case M.lookup refty lookupCache of
                                    Nothing -> onFailure
                                    y       -> return y

          namedResolvers = case prefix of
                               "HEAD"       -> [ RefHead ]
                               "ORIG_HEAD"  -> [ RefOrigHead ]
                               "FETCH_HEAD" -> [ RefFetchHead ]
                               _            -> [ RefTag prefix, RefBranch prefix, RefRemote prefix ]


          tryResolvers :: [IO (Maybe Ref)] -> IO Ref
          tryResolvers []            = return $ fromHexString prefix
          tryResolvers (resolver:xs) = resolver >>= isResolved
             where isResolved (Just r) = return r
                   isResolved Nothing  = tryResolvers xs

          resolvePrePrefix :: IO (Maybe Ref)
          resolvePrePrefix = do
              refs <- findReferencesWithPrefix git prefix
              case refs of
                  []  -> return Nothing
                  [r] -> return (Just r)
                  _   -> error "multiple references with this prefix"

          modf [] ref                  = return (Just ref)
          modf (RevModParent i:xs) ref = do
              parentRefs <- getParentRefs ref
              case i of
                  0 -> error "revision modifier ^0 is not implemented"
                  _ -> case drop (i - 1) parentRefs of
                            []    -> error "no such parent"
                            (p:_) -> modf xs p

          modf (RevModParentFirstN 1:xs) ref = modf (RevModParent 1:xs) ref
          modf (RevModParentFirstN n:xs) ref = do
              parentRefs <- getParentRefs ref
              modf (RevModParentFirstN (n-1):xs) (head parentRefs)
          modf (_:_) _ = error "unimplemented revision modifier"

          getParentRefs ref = do
              obj <- getCommit git ref
              case obj of
                  Just (Commit { commitParents = parents }) -> return parents
                  Nothing -> error "reference in commit chain doesn't exists"

-- | returns a tree from a ref that might be either a commit, a tree or a tag.
resolveTreeish :: Git -> Ref -> IO (Maybe Tree)
resolveTreeish git ref = getObject git ref True >>= mapJustM recToTree where
        recToTree (objectToCommit -> Just (Commit { commitTreeish = tree })) = resolveTreeish git tree
        recToTree (objectToTag    -> Just (Tag tref _ _ _ _))    = resolveTreeish git tref
        recToTree (objectToTree   -> Just t@(Tree _))            = return $ Just t
        recToTree _                                              = return Nothing


-- | Rewrite a set of commits from a revision and returns the new ref.
--
-- If during revision traversal (diving) there's a commit with zero or multiple
-- parents then the traversal will stop regardless of the amount of parent requested.
--
-- calling "rewrite f 2 (revisionOf d)" on the following tree:
--
--          a <-- b <-- c <-- d
--
-- result in the following tree after mapping with f:
--
--          a <-- f(b) <-- f(c) <-- f(d)
--
rewrite :: Git                   -- ^ Repository
        -> (Commit -> IO Commit) -- ^ Mapping function
        -> Revision              -- ^ revision to start from
        -> Int                   -- ^ the number of parents to map
        -> IO Ref                -- ^ return the new head REF
rewrite git mapCommit revision nbParent = do
    ref <- fromMaybe (error "revision cannot be found") <$> resolveRevision git revision
    resolveParents nbParent ref >>= process . reverse

    where resolveParents :: Int -> Ref -> IO [ (Ref, Commit) ]
          resolveParents 0 ref = (:[]) . (,) ref . fromMaybe (error "commit cannot be found") <$> getCommit git ref
          resolveParents n ref = do commit <- fromMaybe (error "commit cannot be found") <$> getCommit git ref
                                    case commitParents commit of
                                         [parentRef] -> liftM ((ref,commit) :) (resolveParents (n-1) parentRef)
                                         _           -> return [(ref,commit)]

          process [] = error "nothing to rewrite"
          process ((_,commit):next) =
                      mapCommit commit >>= looseWrite (gitRepoPath git) . toObject >>= flip rewriteOne next

          rewriteOne prevRef [] = return prevRef
          rewriteOne prevRef ((_,commit):next) = do
                      newCommit <- mapCommit $ commit { commitParents = [prevRef] }
                      ref       <- looseWrite (gitRepoPath git) (toObject newCommit)
                      rewriteOne ref next

-- | build a hierarchy tree from a tree object
buildHTree :: Git -> Tree -> IO HTree
buildHTree git (Tree ents) = mapM resolveTree ents
        where resolveTree (perm, ent, ref) = do
                obj <- getObjectType git ref
                case obj of
                        Just TypeBlob -> return (perm, ent, TreeFile ref)
                        Just TypeTree -> do
                                ctree <- getTree git ref
                                case ctree of
                                        Nothing -> error "unknown reference in tree object: no such child"
                                        Just t  -> do
                                                dir   <- buildHTree git t
                                                return (perm, ent, TreeDir ref dir)
                        Just _        -> error "wrong type embedded in tree object"
                        Nothing       -> error "unknown reference in tree object"

-- | resolve the ref (tree or blob) related to a path at a specific commit ref
resolvePath :: Git            -- ^ repository
            -> Ref            -- ^ commit reference
            -> [ByteString]   -- ^ paths
            -> IO (Maybe Ref)
resolvePath git commitRef paths = do
        commit <- getCommit git commitRef
        case commit of
                Just (Commit { commitTreeish = tree }) -> resolve tree paths
                Nothing                    -> error ("not a valid commit ref: " ++ show commitRef)
        where
                resolve :: Ref -> [ByteString] -> IO (Maybe Ref)
                resolve treeRef []     = return $ Just treeRef
                resolve treeRef (x:xs) = do
                        tree <- getTree git treeRef
                        case tree of
                                Just (Tree ents) -> do
                                        let cEnt = treeEntRef <$> findEnt x ents
                                        if xs == []
                                                then return cEnt
                                                else maybe (return Nothing) (\z -> resolve z xs) cEnt
                                Nothing          -> error ("not a valid tree ref: " ++ show treeRef)

                findEnt x = find (\(_, b, _) -> b == x)
                treeEntRef (_,_,r) = r
