{-# Language ImpredicativeTypes #-}
{-# Language OverloadedStrings #-}
{-# Language DeriveFunctor #-}
module System.FileSystemOperations where

import           Data.IOData(IOData)
import           Conduit
import           Control.Applicative
import           Control.Monad.Free
import           Control.Monad.IO.Class
-- import           Data.Conduit.Combinators(sourceFile, sinkFile)
import           Filesystem hiding (readFile, writeFile)
import qualified Filesystem.Path.CurrentOS as FP

import qualified System.Posix.Files as Posix

import           Prelude hiding (readFile, writeFile)

--------------------------------------------------------------------------------

data Operation path writer reader next = WriteFile       path writer next
                                       | ReadFile        path        (reader -> next)
                                       | DeleteFile      path        next
                                       | CreateDirectory path        next
                                       | ReadDirectory   path        (([path],[path]) -> next) -- ^ Files first, then directories. The returned path is just the *fileName*. Not the full path.

                                       | DeleteDirectory path        next
                                       | Move            path path   next
                                       | Copy            path path   next
                                         deriving (Functor)


type FSOperation path writer reader = Free (Operation path writer reader)


writeFile     :: path -> writer -> FSOperation path writer reader ()
writeFile p w = liftF (WriteFile p w ())

readFile   :: path -> FSOperation path writer reader reader
readFile p = liftF (ReadFile p id)

deleteFile   :: path -> FSOperation path writer reader ()
deleteFile p = liftF (DeleteFile p ())

createDirectory   :: path -> FSOperation path writer reader ()
createDirectory p = liftF (CreateDirectory p ())

readDirectory   :: path -> FSOperation path writer reader ([path],[path])
readDirectory p = liftF (ReadDirectory p id)

deleteDirectory   :: path -> FSOperation path writer reader ()
deleteDirectory p = liftF (DeleteDirectory p ())

move     :: path -> path -> FSOperation path writer reader ()
move p q = liftF (Move p q ())

copy     :: path -> path -> FSOperation path writer reader ()
copy p q = liftF (Copy p q ())




-- testProg = do
--              createDirectory "/Users/frank/tmp/dirtest/foo/bar"











-- | The usual implementation of the operations
interpretIO           :: (IOData ra, IOData wa, MonadResource rm, MonadResource wm)
                      => FSOperation FP.FilePath (Source wm wa) (ConduitM i ra rm ()) b
                      -> wm b
interpretIO (Pure b)  = pure b
interpretIO (Free op) = case op of
    WriteFile p w next       -> do
                                  liftIO . createTree $ FP.dirname p
                                  w $$ sinkFile p
                                  interpretIO next
    ReadFile p    next       -> interpretIO $ next (sourceFile p)
    DeleteFile p next        -> liftIO (removeFile p) >> interpretIO next
    CreateDirectory p next   -> liftIO (createTree p) >> interpretIO next
    ReadDirectory p next     -> do
                                  entries <- liftIO $ listDirectory p
                                  t       <- separateFilesAndDirs p entries
                                  interpretIO $ next t
    DeleteDirectory p next   -> liftIO (removeTree p) >> interpretIO next
    Move p q next            -> liftIO (rename p q)   >> interpretIO next
    Copy p q next            -> liftIO (copyFile p q) >> interpretIO next


-- | We never remove stuff, instead we create new versions of all files in the
--  indicated data directory.
interpretPersistentIO                   :: ( IOData ra, IOData wa
                                           , MonadResource rm, MonadResource wm)
                                        => (FP.FilePath -> wm FP.FilePath)
                                           -- ^ Function that computes a persistent
                                           --     location where we keep the actual file
                                           --     content
                                        -> FSOperation FP.FilePath (Source wm wa)
                                                                   (ConduitM i ra rm ()) b
                                        -> wm b
interpretPersistentIO _       (Pure b)  = pure b
interpretPersistentIO genPath (Free op) = case op of
    WriteFile p w next       -> do
                                  contentPath <- genPath p
                                  -- make sure the directories exist
                                  liftIO . createTree $ FP.dirname contentPath
                                  w $$ sinkFile contentPath
                                  -- make sure the dir containing the link exists
                                  -- and create the link
                                  liftIO $ do createTree $ FP.dirname p
                                              createSymbolicLink contentPath p
                                  interpretPersistentIO genPath next
    ReadFile p    next       -> interpretPersistentIO genPath $ next (sourceFile p)
    DeleteFile p next        -> do
                                  liftIO $ removeLink p
                                  interpretPersistentIO genPath next
    CreateDirectory p next   -> do
                                  contentPath <- genPath p
                                  -- create the directory as well as the
                                  -- content directory
                                  liftIO $ do createTree $ FP.dirname contentPath
                                              createTree p
                                  interpretPersistentIO genPath next
    ReadDirectory p next     -> do
                                  entries <- liftIO $ listDirectory p
                                  t       <- separateFilesAndDirs p entries
                                  interpretPersistentIO genPath $ next t
    DeleteDirectory p next   -> do
                                  q <- genPath p
                                  liftIO (rename p q)
                                  interpretPersistentIO genPath next
                                  -- instead of deleting the tree, we move it to the backup
                                  -- location.
    Move p q next            -> do
                                  contentPathQ <- genPath q
                                  liftIO $ do
                                             -- TODO: This assumes p points to a file!
                                             -- think what to do when p is a dir.
                                             contentPathP <- readSymbolicLink p
                                             createSymbolicLink contentPathP contentPathQ
                                             rename p q
                                  interpretPersistentIO genPath next
    Copy p q next             -> do
                                  contentPathQ <- genPath q
                                  liftIO $ do
                                    b <- isDirectory p
                                    if b then copyTree p q
                                         else do
                                             contentPathP <- readSymbolicLink p
                                             createSymbolicLink contentPathP contentPathQ
                                             createSymbolicLink contentPathQ q
                                  interpretPersistentIO genPath next

  where
    copyTree p q = error "copyTree not implemented yet"




-- incremental :: MonadIO m => FP.FilePath -- ^ contenPath dir
--                          -> FP.FilePath -- ^ baseDir virtual tree
--                          -> FP.FilePath -- ^ Full path to the file for which we want to
--                                         -- know the content path
--                          -> m FP.FilePath
-- incremental contentPath baseDir fp = let Just fp' = stripPrefix baseDir fp
--                                      in do


  -- do






-- | Returns a pair (List of Files, List of Directories)
separateFilesAndDirs              :: (MonadIO m, Applicative m)
                                  => FP.FilePath -> [FP.FilePath]
                                  -> m ([FP.FilePath],[FP.FilePath])
separateFilesAndDirs base entries = foldr collect ([],[]) <$> liftIO (mapM f entries)
  where
    f n = (\b -> if b then Right n else Left n) <$> isDirectory (base FP.</> n)

    collect (Left n) (ls,rs)  = (n:ls,rs)
    collect (Right n) (ls,rs) = (ls,n:rs)



--------------------------------------------------------------------------------


createSymbolicLink     :: FP.FilePath -> FP.FilePath -> IO ()
createSymbolicLink p q = Posix.createSymbolicLink (FP.encodeString p) (FP.encodeString q)


removeLink :: FP.FilePath -> IO ()
removeLink = Posix.removeLink . FP.encodeString

readSymbolicLink :: FP.FilePath -> IO FP.FilePath
readSymbolicLink = fmap FP.decodeString . Posix.readSymbolicLink . FP.encodeString
