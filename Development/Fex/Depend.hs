module Development.Fex.Depend
  ( EnvVar, mkEnvVar, runEnvVar
  , Exe, mkExe, runExe
  , File, mkFile, runFile
  , Dir, mkDir, runDir
  , F.FlagSpec
  , mkFlagBool, mkFlagString, mkFlagInt, mkFlagDouble, mkFlagFile, mkFlagDir
  , runFlagBool, runFlagString, runFlagInt, runFlagDouble, runFlagFile, runFlagDir
  )
where

import System.Directory (findExecutable, doesDirectoryExist, doesFileExist)
import System.Environment (getEnv, lookupEnv)
import System.Process (createProcess, proc)
import Text.Printf (printf)

import Development.Fex.Experiment
import qualified Development.Fex.Flag as F

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

newtype File = File String deriving Show

instance Depend File where
  missing (File path) = do
    exists <- liftIO $ doesFileExist path
    return $ if exists then Nothing else
      Just $ printf "Path '%s' does not exist or is not a file.\n" path

mkFile :: String -- ^ Path to file.
       -> Experiment File
mkFile = dep . File

runFile :: File -> Experiment String
runFile (File f) = return f

newtype Dir = Dir String deriving Show

instance Depend Dir where
  missing (Dir path) = do
    exists <- liftIO $ doesDirectoryExist path
    return $ if exists then Nothing else
      Just $ printf "Path '%s' does not exist or is not a directory.\n" path

mkDir :: String -- ^ Path to directory.
      -> Experiment Dir
mkDir = dep . Dir

runDir :: Dir -> Experiment String
runDir (Dir f) = return f

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
runExe :: [String] -> Exe -> Experiment ()
runExe args (Exe cmd) = liftIO $ do
  r <- createProcess (proc cmd args)
  print "wat"
  return ()


-- Flag dependencies

instance Depend F.FlagSpec where
  missing spec = flagValue (F.long spec) >>= check
    where check :: F.FlagV -> Experiment (Maybe String)
          check (F.FFile s) = mkFile s >>= missing
          check (F.FDir s)  = mkDir s >>= missing
          check _         = return Nothing

mkFlag :: String -> String -> F.FlagV -> Experiment F.FlagSpec
mkFlag name help defv = do
  let spec = F.FlagSpec { F.short = "", F.long = name
                        , F.help = help, F.defv = defv }
  addFlag spec
  dep spec

-- | Declare a dependency on a boolean command line flag. Boolean flags
-- are always satisfied.
mkFlagBool :: String -- ^ The long flag name.
           -> String -- ^ The help message.
           -> Experiment F.FlagSpec
mkFlagBool name help = mkFlag name help $ F.FBool False

-- | Read the value of a boolean flag.
runFlagBool :: F.FlagSpec -> Experiment Bool
runFlagBool (F.FlagSpec { F.long = k }) = flagValue k >>= \flagv ->
  case flagv of F.FBool b -> return b
                _         -> error $ printf "BUG: Flag '%s' is not a bool." k

-- | Declare a dependency on a string command line flag. String flags
-- are always satisfied.
mkFlagString :: String -- ^ The long flag name.
             -> String -- ^ The default value.
             -> String -- ^ The help message.
             -> Experiment F.FlagSpec
mkFlagString name def help = mkFlag name help $ F.FString def

-- | Read the value of a string flag.
runFlagString :: F.FlagSpec -> Experiment String
runFlagString (F.FlagSpec { F.long = k }) = flagValue k >>= \flagv ->
  case flagv of F.FString s -> return s
                _           -> error $ printf "BUG: Flag '%s' is not a string." k

-- | Declare a dependency on an int command line flag. Int flags
-- are always satisfied.
mkFlagInt :: String -- ^ The long flag name.
          -> Int    -- ^ The default value.
          -> String -- ^ The help message.
          -> Experiment F.FlagSpec
mkFlagInt name def help = mkFlag name help $ F.FInt def

-- | Read the value of an int flag.
runFlagInt :: F.FlagSpec -> Experiment Int
runFlagInt (F.FlagSpec { F.long = k }) = flagValue k >>= \flagv ->
  case flagv of F.FInt s -> return s
                _        -> error $ printf "BUG: Flag '%s' is not an int." k

-- | Declare a dependency on a double command line flag. Double flags
-- are always satisfied.
mkFlagDouble :: String -- ^ The long flag name.
          -> Double    -- ^ The default value.
          -> String -- ^ The help message.
          -> Experiment F.FlagSpec
mkFlagDouble name def help = mkFlag name help $ F.FDouble def

-- | Read the value of a double flag.
runFlagDouble :: F.FlagSpec -> Experiment Double
runFlagDouble (F.FlagSpec { F.long = k }) = flagValue k >>= \flagv ->
  case flagv of F.FDouble s -> return s
                _           -> error $ printf "BUG: Flag '%s' is not a double." k

-- | Declare a dependency on a file command line flag. File flags are only
-- satisfied when they refer to files that exist. (To accept a file that
-- does not exist, use a string flag.)
mkFlagFile :: String -- ^ The long flag name.
           -> String -- ^ The default file.
           -> String -- ^ The help message.
           -> Experiment F.FlagSpec
mkFlagFile name def help = mkFlag name help $ F.FFile def

-- | Read the value of a file flag.
runFlagFile :: F.FlagSpec -> Experiment String
runFlagFile (F.FlagSpec { F.long = k }) = flagValue k >>= \flagv ->
  case flagv of F.FFile s -> return s
                _         -> error $ printf "BUG: Flag '%s' is not a file." k

-- | Declare a dependency on a directory command line flag. Directory flags are 
-- only satisfied when they refer to directories that exist. (To accept
-- a directory that does not exist, use a string flag.)
mkFlagDir :: String -- ^ The long flag name.
          -> String -- ^ The default directory.
          -> String -- ^ The help message.
          -> Experiment F.FlagSpec
mkFlagDir name def help = mkFlag name help $ F.FDir def

-- | Read the value of a directory flag.
runFlagDir :: F.FlagSpec -> Experiment String
runFlagDir (F.FlagSpec { F.long = k }) = flagValue k >>= \flagv ->
  case flagv of F.FDir s -> return s
                _        -> error $ printf "BUG: Flag '%s' is not a directory." k
