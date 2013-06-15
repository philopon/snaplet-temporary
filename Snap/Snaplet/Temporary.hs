{-#LANGUAGE TemplateHaskell, FlexibleContexts, OverloadedStrings  #-}
module Snap.Snaplet.Temporary(Temporary, HasTemporary(..), initTemporary, temporaryFile) where

import System.IO
import System.Directory

import Control.Applicative
import Control.Monad.State
import Control.Concurrent.STM
import Control.Concurrent
import Control.Lens

import Snap.Snaplet

import Data.IORef
import qualified Data.Text as T
import qualified Data.HashSet as S
import Data.Hashable

data File =
  File { _path   :: FilePath
       , _handle :: Handle
       , _thread :: ThreadId
       }
  | Dummy {_path :: FilePath }
  deriving Show
makeLenses ''File

instance Hashable File where
  hashWithSalt s f = hashWithSalt s (f ^. path)

instance Eq File where
  a == b = a^.path == b^.path

data Temporary =
  Temporary { _directory :: IORef FilePath
            , _files     :: TVar (S.HashSet File)
            }
makeLenses ''Temporary

--------------------------------------------------------------------------------
class HasTemporary a where
  temporaryLens :: SnapletLens (Snaplet a) Temporary

removeTmp :: Temporary -> FilePath -> Handle -> IO ()
removeTmp ref pth hndl = do
  hIsOpen hndl      >>= \o -> when o $ hClose hndl
  doesFileExist pth >>= \e -> when e (removeFile pth)
  atomically $ modifyTVar' (ref^.files) (S.delete $ Dummy pth)

removeAll :: TVar (S.HashSet File) -> IO ()
removeAll fls = do
  s <- atomically $ readTVar fls <* writeTVar fls S.empty
  forM_ (S.toList s) $ \(File f h t) -> do
    killThread t
    hIsOpen h       >>= \o -> when o $ hClose h
    doesFileExist f >>= \e -> when e (removeFile f)

initTemporary :: SnapletInit b Temporary
initTemporary = makeSnaplet "tmp" "temporary file provider" Nothing $ do
  name <- maybe "tmp" T.unpack <$> getSnapletName
  dir  <- liftIO $ newIORef name
  fs   <- liftIO $ newTVarIO S.empty

  onUnload $ removeAll fs
  return $ Temporary dir fs

temporaryFile :: (MonadSnaplet m, MonadIO (m b v), MonadState Temporary (m b Temporary), HasTemporary b) =>
                 Int -> String -> m b v (FilePath, Handle)
temporaryFile expire name = do
  ref    <- withTop' temporaryLens get
  dir    <- liftIO $ readIORef $ ref ^. directory
  file   <- liftIO $ openTempFile dir name

  liftIO $ do pid <- forkIO $ sentinel ref file
              atomically $ modifyTVar' (ref^.files) (S.insert $ uncurry File file pid)
  
  return file
  where sentinel ref file = threadDelay expire >> uncurry (removeTmp ref) file
          


