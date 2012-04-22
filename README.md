Hit
===

Hit is a reimplementation of some git operations in pure haskell.


what it does do:

* read loose objects, and packed objects.
* write new loose objects
* git like operations available: commit, cat-file, verify-pack, rev-list, ls-tree.

what is doesn't do:

* reimplement the whole of git.
* checkout's index reading/writing, fetching, merging, diffing.

The main functions for users are available from the Data.Git.Repository module.

The essential functions are:

* withRepo: create a new git context and execute a function with the context. functional equivalent of withFile but for git repository.
* resolveRevision: turns a git revision (e.g. HEAD, 0a24^^^~3) into a SHA1 reference.
* resolvePath: from a commit ref and a path, it will gives the tree or blob reference of the object at the specific path (see example).
* findObject: from a SHA1 reference, gives a high level object (Commit, Blob, Tree, Tag, Delta) from the git repository. if called with resolveDelta set, it will resolves deltas to be simple objects with the deltas applied.
* findObjectRaw: similar to findObject but gives a raw representation (lazy bytestring) of the object.

API Example
-----------

resolving path of the README file and returning the reference to the blob :

    {-# LANGUAGE OverloadedStrings #-}
    import Data.Git.Repository

    showPathRef commitRef = withRepo ".git" $ \git -> do
        ref <- maybe (error "inexistent object at this path") id `fmap` resolvePath git commitRef ["README"]
        putStrLn ("README has the reference: " ++ show ref)


catting an object from a ref:

    import Data.Git.Repository

    catFile ref = withRepo ".git" $ \git -> do
        obj <- maybe (error "not a valid object") id `fmap` findObjectRaw git ref True
        L.putStrLn (oiData obj)


more examples on how to use api can be found in Hit.hs.
