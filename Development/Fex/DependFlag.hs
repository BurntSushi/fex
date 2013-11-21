module Development.Fex.DependFlag
  ( F.FlagSpec
  , mkFlagBool, mkFlagString, mkFlagInt, mkFlagDouble, mkFlagFile, mkFlagDir
  , runFlagBool, runFlagString, runFlagInt, runFlagDouble, runFlagFile, runFlagDir
  )
where

import System.Directory (doesDirectoryExist, doesFileExist)
import Text.Printf (printf)

import Development.Fex.Experiment
import qualified Development.Fex.Flag as F

instance Depend F.FlagSpec where
  missing spec = flagValue (F.long spec) >>= check
    where check :: F.FlagV -> Experiment (Maybe String)
          check (F.FFile s) = do
            exists <- liftIO $ doesFileExist s
            err exists s "file"
          check (F.FDir s)  = do
            exists <- liftIO $ doesDirectoryExist s
            err exists s "directory"
          check _         = return Nothing

          err :: Bool -> String -> String -> Experiment (Maybe String)
          err exists path typ =
            return $ if exists then Nothing else
              Just $ printf "Path '%s' does not exist or is not a %s.\n" path typ

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
