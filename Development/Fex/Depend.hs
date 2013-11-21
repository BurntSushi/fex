module Development.Fex.Depend
  ( module Development.Fex.DependFlag
  , EnvVar, mkEnvVar, runEnvVar
  , Exe, mkExe, runExe
  )
where

import System.Directory (findExecutable)
import System.Environment (getEnv, lookupEnv)
import Text.Printf (printf)

import Development.Fex.Experiment
import Development.Fex.DependFlag

-- | An environment variable dependency. Currently, an environment variable
-- can only be read.
--
-- The existence of the environment variable is checked at program startup.
-- If it disappears during execution, a runtime error will be raised.
newtype EnvVar = EnvVar String deriving Show

instance Depend EnvVar where
  missing (EnvVar v) = do
    v' <- liftIO $ lookupEnv v
    case v' of
      Nothing -> return $ Just
                 $ printf "Environment variable '%s' is not defined." v
      _       -> return Nothing

-- | Declare a dependency on a particular environment variable.
mkEnvVar :: String -- ^ Name of environment variable.
         -> Experiment EnvVar
mkEnvVar = dep . EnvVar

-- | Reads the current value of the environment variable.
-- If it no longer exists, a run time error will occur.
runEnvVar :: EnvVar -> Experiment String
runEnvVar (EnvVar v) = liftIO $ getEnv v

-- | A executable dependency. Currently, only the name is required. The
-- system's PATH variable will be searched to determined if the dependency
-- exists.
newtype Exe = Exe String deriving Show

instance Depend Exe where
  missing (Exe cmd) = do
    r <- liftIO $ findExecutable cmd
    case r of
      Nothing -> return $ Just $ printf "Could not find executable '%s'" cmd
      _       -> return Nothing

-- | Declare a dependency on a particular executable in the environment.
mkExe :: String -> Experiment Exe
mkExe s = mkEnvVar "PATH" >> dep (Exe s)

-- | Run an executable in the current environment.
--
-- INCOMPLETE.
runExe :: [String] -> Exe -> Experiment String
runExe _ (Exe cmd) = liftIO $ do
  printf "Running command '%s'...\n" cmd
  return $ "result of '" ++ cmd ++ "'"

