{-# LANGUAGE TypeFamilies #-}
module Development.Fex.Depends where

import Control.Monad.IO.Class (MonadIO(..))
import System.Directory (findExecutable)
import Text.Printf (printf)

import Development.Fex.Experiment

newtype Exe = Exe String deriving Show

instance Depend Exe where
  type Args = String
  depend = return . Exe
  missing (Exe cmd) = do
    r <- findExecutable cmd
    case r of
      Nothing -> return $ Just $ printf "Could not find executable '%s'" cmd
      _       -> return Nothing

runExe :: Exe -> Experiment String
runExe (Exe cmd) = liftIO $ do
  printf "Running command '%s'...\n" cmd
  return $ "result of '" ++ cmd ++ "'"

