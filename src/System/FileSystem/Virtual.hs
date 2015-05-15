{-# Language FlexibleContexts #-}
{-# Language LambdaCase #-}
{-# Language MultiParamTypeClasses #-}
{-# Language GADTs #-}
{-# Language DataKinds #-}
module System.FileSystem.Virtual where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Free
import           Control.Monad.State.Class
import           Data.Default
import           Data.FileSystem
import qualified Data.FileSystem as VFS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import           System.FileSystemOperations

type NonEmptyPath = NE.NonEmpty FileName
type ErrorMessage = String

interpretVirtual          :: ( Measured v a, Default a
                             , MonadState (FSTree D v a) m
                             , MonadError ErrorMessage m
                             , Functor m
                             )
                          => FSOperation NonEmptyPath a a b -> m b
interpretVirtual (Pure b) = return b
interpretVirtual (Free op) = case op of
    WriteFile p w next       -> do
                                  let (p',n) = andLast p
                                  modify (updateAt p' n (set fileData w))
                                  interpretVirtual next
    ReadFile p    next       -> withFileOrDirectory p >>= \case
                                  Nothing        -> throwError $ "readFile: " ++ noSuch p
                                  Just (Left f)  -> interpretVirtual (next $ _fileData f)
                                  Just (Right d) -> interpretVirtual (next $ _fileData d)
    DeleteFile p next        -> let (p',n) = andLast p
                                in modify (delete p' n) >> interpretVirtual next
    CreateDirectory p next   -> do
                                  let (p',n) = andLast p
                                  modify (assignTo p' (emptyDirectory n def))
                                  interpretVirtual next
    ReadDirectory p next     -> withFileOrDirectory p >>= \case
                                  Nothing -> throwError $ "readDirectory: " ++ noSuch p
                                  Just (Left _) -> throwError $
                                     "readDirectory: Found a file, Expected a directory"
                                  Just (Right d) ->
                                    let
                                        f      = map ((NE.:| []) . _fileName)
                                        mkList = map (\(k,v) -> v k) . M.toAscList
                                        fs = f . mkList $ files d
                                        ds = f . mkList $ subDirectories d
                                    in interpretVirtual $ next (fs,ds)
    DeleteDirectory p next   -> let (p',n) = andLast p
                                in modify (delete p' n) >> interpretVirtual next
    Move p q next            -> moveOrCopy VFS.move p q next
    Copy p q next            -> moveOrCopy VFS.copy p q next
  where
    moveOrCopy f p q next = let oldPath = andLast p
                                newPath = andLast q
                            in do
                                 mt <- f oldPath newPath <$> get
                                 case mt of
                                   Nothing -> throwError $ "move/copy: " ++ noSuch p
                                   Just t  -> (put t >> interpretVirtual next)


noSuch p = "No such file or directory " ++ show p


withFileOrDirectory   :: (MonadState (FSTree D v a) m, Functor m)
                      => NonEmptyPath -> m (Maybe (FileOrDir v a))
withFileOrDirectory p = let (p',n) = andLast p
                        in accessFileOrDirectoryAt p' n <$> get





andLast                    :: NE.NonEmpty a -> ([a],a)
andLast (x NE.:| [])       = ([],x)
andLast (x NE.:| (y : ys)) = let (is,l) = andLast (y NE.:| ys) in (x:is,l)
