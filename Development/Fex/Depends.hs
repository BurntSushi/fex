module Development.Fex.Depends where

import Control.Monad.IO.Class (MonadIO(..))
import System.Directory (findExecutable)
import System.Environment (getEnv, lookupEnv)
import Text.Printf (printf)

import Development.Fex.Experiment

newtype EnvVar = EnvVar String deriving Show

instance Depend EnvVar where
  missing (EnvVar v) = do
    v' <- lookupEnv v
    case v' of
      Nothing -> return $ Just
                 $ printf "Environment variable '%s' is not defined." v
      _       -> return Nothing

mkEnvVar :: String -> Experiment EnvVar
mkEnvVar = dep . EnvVar

runEnvVar :: EnvVar -> Experiment String
runEnvVar (EnvVar v) = liftIO $ getEnv v

newtype Exe = Exe String deriving Show

instance Depend Exe where
  missing (Exe cmd) = do
    r <- findExecutable cmd
    case r of
      Nothing -> return $ Just $ printf "Could not find executable '%s'" cmd
      _       -> return Nothing

mkExe :: String -> Experiment Exe
mkExe s = dep (Exe s)

runExe :: Exe -> Experiment String
runExe (Exe cmd) = do
  v <- mkEnvVar "PATH" >>= runEnvVar
  liftIO $ do
    printf "Running command '%s'...\n" cmd
    return $ "result of '" ++ cmd ++ "'"

