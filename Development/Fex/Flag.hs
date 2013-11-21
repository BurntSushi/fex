module Development.Fex.Flag
  ( Config
  , Flags
  , FlagSpec(..)
  , FlagV(..)
  , configIO
  , addFlag
  , addFlagValue
  , emptyFlags
  , emptyConfig
  , appendFlags
  , appendConfig
  , flagLookup
  , confLookup
  , defaultConfig
  )
where

import Control.Monad (when)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate)
import qualified Data.Map as M
import qualified System.Console.GetOpt as O
import System.Exit (exitFailure)
import System.IO (stderr)
import Text.Printf (HPrintfType, hPrintf, printf)

type Config = M.Map String FlagV

type Flags = M.Map String FlagSpec

data FlagSpec = FlagSpec { short :: String, long :: String, help :: String
                         , defv :: FlagV
                         }

instance Show FlagSpec where
  show (FlagSpec { long = long, defv = defv }) =
    printf "Flag %s [%s]" long (show defv)

data FlagV = FBool Bool -- default should always be false
           | FString String
           | FInt Int
           | FDouble Double
           | FFile String
           | FDir String

instance Show FlagV where
  show (FBool b) = printf "BOOL %s" $ show b
  show (FString s) = printf "STRING %s" s
  show (FInt n) = printf "INT %d" n
  show (FDouble f) = printf "DOUBLE %f" f
  show (FFile f) = printf "FILE %s" f
  show (FDir f) = printf "DIR %s" f

showHelp :: FlagV -> String
showHelp (FBool _) = "BOOL (default: false)"
showHelp (FString s) = printf "STRING (default: %s)" s
showHelp (FInt n) = printf "INT (default: %d)" n
showHelp (FDouble f) = printf "DOUBLE (default: %f)" f
showHelp (FFile f) = printf "FILE (default: %s)" f
showHelp (FDir f) = printf "DIR (default: %s)" f

emptyFlags :: Flags
emptyFlags = M.fromList
  [ ("help", FlagSpec { short = "h", long = "help", defv = FBool False
                      , help = "Show this help message."
                      })
  -- , ("depends", FlagSpec { short = "", long = "depends", defv = FBool False 
                         -- , help = "Show a list of dependencies and exit\n" 
                                  -- ++ "without evaluating any experiments." 
                         -- }) 
  ]

emptyConfig :: Config
emptyConfig = M.empty

defaultConfig :: Flags -> Config
defaultConfig = M.fromList . map pair . M.elems
  where pair s = (long s, defv s)

addFlag :: FlagSpec -> Flags -> Flags
addFlag spec = M.insert (long spec) spec

addFlagValue :: String -> FlagV -> Config -> Config
addFlagValue = M.insert

appendFlags :: Flags -> Flags -> Flags
appendFlags = M.union

appendConfig :: Config -> Config -> Config
appendConfig = M.union

flagLookup :: String -> Flags -> Maybe FlagSpec
flagLookup = M.lookup

confLookup :: String -> Config -> Maybe FlagV
confLookup = M.lookup

-- | Parse command line arguments according to a specification.
-- The program terminates if there is an error parsing.
configIO :: Flags -- ^ Command line specification.
         -> [String] -- ^ Command line arguments.
         -> IO Config
configIO flags args =
  case O.getOpt O.Permute (toOptSpec flags) args of
    (vals, [], []) -> do
      let conf = M.fromList vals `appendConfig` defaultConfig flags
      when ("help" `isset` conf) $ usageExit flags
      return conf
    (_, _, errs@(_:_)) -> do
      ef "Error(s) parsing flags:\n\t%s\n" $
        intercalate "\n\t" $ map strip errs
      exitFailure
    (_, _, []) -> do
      ef "fex does not have any positional arguments.\n\n"
      usageExit flags

isset :: String -> Config -> Bool
isset k conf = case M.lookup k conf of
                    Nothing -> False
                    Just flagv ->
                      case flagv of FBool True -> True
                                    _          -> False

toOptSpec :: Flags -> [O.OptDescr (String, FlagV)]
toOptSpec = map convert . M.elems
  where
    convert :: FlagSpec -> O.OptDescr (String, FlagV)
    convert spec = O.Option (short spec) [long spec]
                            (arg $ defv spec) (help spec)
      where
        addKey f s = (long spec, f s)

        arg :: FlagV -> O.ArgDescr (String, FlagV)
        arg (FBool _) = O.NoArg (long spec, FBool True)
        arg h@(FString _) = O.ReqArg (addKey FString) (showHelp h)
        arg h@(FInt _) = O.ReqArg (addKey $ FInt . read) (showHelp h)
        arg h@(FDouble _) = O.ReqArg (addKey $ FDouble . read) (showHelp h)
        arg h@(FFile _) = O.ReqArg (addKey FFile) (showHelp h)
        arg h@(FDir _) = O.ReqArg (addKey FDir) (showHelp h)

usageExit :: Flags -> IO a
usageExit flags = usage flags >> exitFailure

usage :: Flags -> IO a
usage flags = ef "%s\n" $ O.usageInfo "Usage: fex [flags]" $ toOptSpec flags

ef :: HPrintfType r => String -> r
ef = hPrintf stderr

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace
