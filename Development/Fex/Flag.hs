module Development.Fex.Flag
  ( Config
  , Flags
  , configIO
  , addFlag
  , emptyFlags
  , emptyConfig
  , appendFlags
  , appendConfig
  , flagLookup
  )
where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified System.Console.GetOpt as O
import System.Exit (exitFailure)
import System.IO (stderr)
import Text.Printf (HPrintfType, hPrintf)

type Config = M.Map String String

type Flags = [O.OptDescr (String, String)]

emptyFlags :: Flags
emptyFlags = []

emptyConfig :: Config
emptyConfig = M.empty

appendFlags :: Flags -> Flags -> Flags
appendFlags = (++)

appendConfig :: Config -> Config -> Config
appendConfig = M.union

flagLookup :: String -> Config -> String
flagLookup k = fromMaybe "" . M.lookup k

configIO :: Flags -> [String] -> IO Config
configIO flags args =
  case O.getOpt O.Permute flags args of
    (vals, [], []) -> return $ M.fromList vals
    (_, _, errs@(_:_)) -> do
      ef "Error(s) parsing flags:\n\t%s\n" $
        intercalate "\n\t" $ map strip errs
      exitFailure
    (_, _, []) -> do
      ef "fex does not have any positional arguments.\n\n"
      usageExit flags

addFlag :: String -- ^ The long form flag name.
        -> String -- ^ The type of value accepted (human readable).
        -> String -- ^ Description of flag.
        -> Flags  -- ^ Given flags.
        -> Flags  -- ^ Given set of flags plus the flag specified.
addFlag name tval help flags = flag : flags
  where flag = O.Option "" [name] req help
        req  = O.ReqArg (\v -> (name, v)) tval

usageExit :: Flags -> IO a
usageExit flags = usage flags >> exitFailure

usage :: Flags -> IO a
usage flags = ef "%s\n" $ O.usageInfo "Usage: erd [flags]" flags

ef :: HPrintfType r => String -> r
ef = hPrintf stderr

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace
