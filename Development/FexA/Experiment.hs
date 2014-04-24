{-# LANGUAGE FlexibleContexts, FlexibleInstances, Rank2Types, TypeSynonymInstances #-}
module Development.FexA.Experiment
  (

  -- * Fex language types
    Fex
  , Value(..)
  , Dependency
  , Flag(..)

  -- ** Introducing basic terms
  , lit, var, app, lam, list

  -- ** Introducing values
  , bool, int, double, str, pureLam

  -- ** Introducing flag terms
  , flag

  -- ** Introducing dependency terms
  , readEnvVar, runExe, file, dir

  -- * Evaluating a Fex term
  , EvalContext
  , eval, evalIO, evalIO', evalPure, applyFlags

  -- * Dependency analysis
  , DepTree, depTree, staticDeps, showStatic
  , depMissing, depStatus
  )
where

import Control.Monad.Identity (Identity, runIdentity)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate, nub)
import Data.Maybe (fromMaybe, isJust)
import System.Directory (doesDirectoryExist, findExecutable, doesFileExist)
import System.Environment (getEnv, lookupEnv)
import System.Exit (exitFailure)
import System.Process (readProcess)
import Text.Printf (PrintfArg, printf)

-- | Fex is basically the untyped lambda calculus with one addition: a special
-- representation for terms corresponding to dependencies.
data Fex = Lit Value
         | Var Var
         | List [Fex]
         | App Fex Fex
         | Lam Fex
         | Dep Dependency
         | Flag Flag

instance Show Fex where
  show (Lit v) = "Lit " ++ show v
  show (Var v) = "Var " ++ show v
  show (Dep d) = "Dep " ++ show d
  show (Flag f) = "Flag " ++ show f
  show (App e1 e2) = "App " ++ show e1 ++ " " ++ show e2
  show (Lam e) = "Lam " ++ show e
  show (List es) = "List " ++ show es

-- | De Bruijn indexing.
data Var = VZ | VS Var deriving Show

-- | The only values in Fex are integers, doubles, bools, strings, lists
-- and pure functions.
data Value = Bool Bool
           | Int Int
           | Double Double
           | String String
           | VList [Value]
           | Pure (Value -> Value)

instance Eq Value where
  (Bool b1) == (Bool b2) = b1 == b2
  (Int n1) == (Int n2) = n1 == n2
  (Double d1) == (Double d2) = d1 == d2
  (String s1) == (String s2) = s1 == s2
  (VList vs1) == (VList vs2) = vs1 == vs2
  _ == _ = False

instance Show Value where
  show (Bool b) = show b
  show (Int n) = show n
  show (Double d) = show d
  show (String s) = s
  show (VList vs) = show vs
  show (Pure _) = "<closure>"

-- | A closed representation of all available dependencies. It is not
-- extensible.
data Dependency = ReadEnvVar Fex
                | RunExe Fex
                | File Fex
                | Dir Fex
                deriving Show

-- | A representation of a command line flag. Only simple flags are
-- available. Namely, bool, integer, double or string flags. Each flag
-- must have a default value and optionally include a help message.
data Flag = FBool   String String        -- ^ Name and help.
                                         -- Default is always false.
          | FInt    String Int String    -- ^ Name, default and help.
          | FDouble String Double String -- ^ Name, default and help.
          | FString String String String -- ^ Name, default and help.
          deriving Show

-- | An environment for holding Fex values.
-- Note that this includes closures, which only exist as an intermediate form.
type Env m = [EvalValue m]

type Closure m = EvalValue m -> m (EvalValue m)

-- | A value returned by evaluation can either be a Fex value or a closure.
data EvalValue m = Closure (Closure m) | V Value

instance Show (EvalValue m) where
  show (Closure _) = "<closure>"
  show (V v) = show v

asValue :: EvalValue m -> Value
asValue (V v) = v
asValue _ = error "Expected value but got closure."

lookv :: EvalContext m => Var -> Env m -> EvalValue m
lookv v env = fromMaybe (errf "Unbound variable '%s'" (show v)) (lookv' v env)
  where lookv' _ [] = Nothing
        lookv' VZ (x:_) = Just x
        lookv' (VS v) (_:xs) = lookv' v xs

-- | Replaces flag values specified on the command line with their
-- corresponding `Flag` terms in the Fex term given. All other `Flag` terms
-- are ignored (they will evaluate to default values).
applyFlags :: [String] -> Fex -> Fex
applyFlags _ = id

-- | Just like `evalIO`, except it stops the program and outputs an error
-- message if one occurs.
evalIO' :: Fex -> IO Value
evalIO' e = evalIO e >>= \e' ->
              case e' of
                Left err -> putStrLn err >> exitFailure
                Right v  -> return v

-- | Completely evaluate a Fex term to a value after checking if the static
-- dependencies of the given term exist. If not, then an error message is
-- returned detailing each missing dependency.
evalIO :: Fex -> IO (Either String Value)
evalIO e = do
  stats <- fmap (zip $ staticDeps e) $ mapM depMissing $ staticDeps e
  let missing = filter (isJust . snd) stats
  if null missing
    then eval e >>= \v -> return $ Right v
    else return $ Left $ intercalate "\n" $ map depStatus missing

-- | Evaluate a Fex term as a pure computation. This results in a
-- run time error if the Fex term uses any dependencies.
evalPure :: Fex -> Value
evalPure = runIdentity . eval

-- | Evaluate a Fex term in any evaluation context.
eval :: EvalContext m => Fex -> m Value
eval = evalEnv []

-- | Evaluate a Fex term in any evaluation context with the given environment.
-- Note that this should only be used when providing your own implementation
-- of `EvalContext`.
evalEnv :: EvalContext m => Env m -> Fex -> m Value
evalEnv env e = eval' env e >>= \e' ->
                  case e' of
                    V v -> return v
                    Closure _ -> error "Evaluation must end with Value type."

eval' :: EvalContext m => Env m -> Fex -> m (EvalValue m)
eval' env = ev
  where ev (Lit v) = return $ V v
        ev (Var v) = return $ lookv v env
        ev (Dep d) = deval d ev
        ev (Flag f) = return $ V $ feval f
        ev (List es) = mapM ev es >>= \vs -> return $ V $ VList $ map asValue vs
        ev (App f v) = do
          f' <- ev f
          v' <- ev v
          case f' of
            Closure f'' -> f'' v'
            V (Pure f'') -> return $ V $ f'' $ asValue v'
            _ -> errf "Non-function value in LHS of app: %s" (show f')
        ev (Lam body) = return $ Closure $ \v -> eval' (v:env) body

        -- This always uses the default value.
        feval :: Flag -> Value
        feval (FBool   _ _)   = Bool False
        feval (FInt    _ d _) = Int d
        feval (FDouble _ d _) = Double d
        feval (FString _ d _) = String d

-- | An evaluation context determines how certain terms (like dependencies)
-- are evaluated. For example, in a pure computation, a dependency is not
-- runnable since it, by definition, requires some sort of IO.
--
-- At this time, it is not possible to define your own evaluation context.
class Monad m => EvalContext m where
  -- | Evaluates a dependency in a particular context.
  deval :: Dependency -- ^ The dependency to evaluate.
        -> (Fex -> m (EvalValue m)) -- ^ An evaluator.
        -> m (EvalValue m) -- ^ The result of evaluating a dependency.

-- | A pure evaluation causes a run time error if it tries to evaluate a
-- dependency.
instance EvalContext Identity where
  deval d _ = errf "Dependency can't be evaluated in pure context: %s" (show d)

-- | An evaluation in the IO monad executes the IO action indicated by the
-- dependency. (e.g., Reading an environment variable, executing a process,
-- etc.)
instance EvalContext IO where
  deval (ReadEnvVar e) ev = do
    v <- ev e >>= (getEnv . show . asValue)
    return $ V $ String v
  deval (RunExe exe) ev = do
    exe' <- fmap (show . asValue) $ ev exe
    return $ Closure $ \args -> do
      let args' = map show $ asList $ asValue args
      fmap (V . String) $ readProcess exe' args' ""
  deval (File path) ev = ev path
  deval (Dir path) ev = ev path

asList :: Value -> [Value]
asList (VList vs) = vs
asList v = errf "Expected list value, but got '%s'." (show v)
    
errf :: PrintfArg r => String -> r -> a
errf s r = error $ printf s r

-- | A dependency tree represents all dependencies used in a Fex term.
data DepTree = Node (Maybe Dependency) [DepTree]
-- By construction, all dependencies of the form @Node (Just d) []@ are
-- static dependencies. Leaves are represented by @Node Nothing []@.

depLeaf :: DepTree
depLeaf = Node Nothing []

depJoin :: DepTree -> DepTree -> DepTree
depJoin t1 t2 = Node Nothing [t1, t2]

instance Show DepTree where
  show = show' 0
    where show' :: Int -> DepTree -> String
          show' _ (Node Nothing []) = ""
          show' depth (Node Nothing ts) =
            printf "%s%s" (dstr depth) (subdeps depth ts)
          show' depth (Node (Just d) []) =
            printf "%s%s" (dstr depth) (showStatic d)
          show' depth (Node (Just d) ts) =
            printf "%s%s\n%s%s" (dstr depth) (showDynamic d)
                                (dstr depth) (subdeps (depth+1) ts)
          
          dstr :: Int -> String
          dstr n = concat $ replicate n "  "

          subdeps :: Int -> [DepTree] -> String
          subdeps depth = intercalate "\n" . map (show' depth)

-- | Tidies a depdency tree so that all leaves are minimal. e.g., A leaf of
-- the form @Node Nothing [Node Nothing []]@ is rewritten as @Node Nothing []@.
depTidy :: DepTree -> DepTree
depTidy (Node d ts) = Node d $ filter (not . isNil) $ map depTidy ts
  where isNil (Node Nothing []) = True
        isNil _ = False

-- | Constructs a dependency tree from a Fex term.
depTree :: Fex -> DepTree
depTree = depTidy . depTree'
  where depTree' :: Fex -> DepTree
        depTree' (Lit _) = depLeaf
        depTree' (Var _) = depLeaf
        depTree' (List es) = foldl depJoin depLeaf $ map depTree' es
        depTree' (App f v) = depTree' f `depJoin` depTree' v
        depTree' (Lam body) = depTree' body
        depTree' (Dep d) = dt d
        depTree' (Flag _) = depLeaf

        dt :: Dependency -> DepTree
        dt d@(ReadEnvVar e) = Node (Just d) [depTree' e]
        dt d@(RunExe exe) = Node (Just d) [depTree' exe]
        dt d@(File path) = Node (Just d) [depTree' path]
        dt d@(Dir path) = Node (Just d) [depTree' path]

-- | Returns a list of all static dependencies in a Fex term. Duplicates are
-- excluded.
staticDeps :: Fex -> [Dependency]
staticDeps = nub . sdeps . depTree
  where sdeps (Node (Just d) []) = [d]
        sdeps (Node _ ts) = concatMap sdeps ts

instance Eq Dependency where
  (ReadEnvVar e1) == (ReadEnvVar e2) = evalPure e1 == evalPure e2
  (RunExe e1) == (RunExe e2) = evalPure e1 == evalPure e2
  (File e1) == (File e2) = evalPure e1 == evalPure e2
  (Dir e1) == (Dir e2) = evalPure e1 == evalPure e2
  _ == _ = False

showDynamic :: Dependency -> String
showDynamic (ReadEnvVar _) = "ReadEnvVar"
showDynamic (RunExe _) = "RunExe"
showDynamic (File _) = "File"
showDynamic (Dir _) = "Dir"

-- | Returns a string representation of a static dependency.
-- A run time error will occur if given a dynamic dependency.
showStatic :: Dependency -> String
showStatic (ReadEnvVar e) = printf "ReadEnvVar '%s'" $ show $ evalPure e
showStatic (RunExe exe) = printf "RunExe '%s'" $ show $ evalPure exe
showStatic (File path) = printf "File '%s'" $ show $ evalPure path
showStatic (Dir path) = printf "Dir '%s'" $ show $ evalPure path

-- | If a dependency is not present, a string is returned explaining why.
-- This can be used on dynamic and static dependencies, but if a dynamic
-- dependency is used, all sub-dependencies are evaluated.
depMissing :: Dependency -> IO (Maybe String)
depMissing (ReadEnvVar e) = do
  name <- fmap show $ eval e
  exists <- lookupEnv name
  return $ case exists of
    Nothing -> Just $ printf "Environment variable '%s' is not defined." name
    _       -> Nothing
depMissing (RunExe exe) = do
  cmd <- fmap show $ eval exe
  exists <- findExecutable cmd
  return $ case exists of
    Nothing -> Just $ printf "Could not find executable '%s'." cmd
    _       -> Nothing
depMissing (File path) = do
  path' <- fmap show $ eval path
  exists <- doesFileExist path'
  return $ if exists then Nothing else
    Just $ printf "Path '%s' does not exist or is not a file." path'
depMissing (Dir path) = do
  path' <- fmap show $ eval path
  exists <- doesDirectoryExist path'
  return $ if exists then Nothing else
    Just $ printf "Path '%s' does not exist or is not a directory." path'

-- | Convert a static dependency and its status to a human readable string.
-- A run time error will occur if a dependency is not static.
depStatus :: (Dependency, Maybe String) -> String
depStatus (d, Nothing)  = showStatic d ++ " ... OK."
depStatus (d, Just err) = showStatic d ++ " ... Not found!\n" ++ indent err
  where indent = dropWhileEnd isSpace . unlines . map ("    " ++) . lines

-- | Introduce a bool Fex term.
bool :: Bool -> Fex
bool = Lit . Bool

-- | Introduce an integer Fex term.
int :: Int -> Fex
int = Lit . Int

-- | Introduce a double Fex term.
double :: Double -> Fex
double = Lit . Double

-- | Introduce a string Fex term.
str :: String -> Fex
str = Lit . String

-- | Introduce a pure computation over Fex values.
pureLam :: (Value -> Value) -> Fex
pureLam = Lit . Pure

-- | Introduce a literal Fex term.
lit :: Value -> Fex
lit = Lit

-- | Introduce a lambda bound variable Fex term.
-- Note that this uses de Bruijn indexing. In particular, a variable is
-- referenced by a natural number which indicates the number of binders that
-- are in scope between the variable and its corresponding binder.
--
-- Examples in lambda calculus with variable names:
--
-- > \x.\y.x
-- > \x.\y.\z.x z (y z)
-- > \z.(\y.y (\x.x)) (\x.z x)
--
-- Corresponding terms with de Bruijn indexing:
--
-- > \.\.2
-- > \.\.\.3 1 (2 1)
-- > \.(\.1 (\.1)) (\.2 1)
--
-- For completeness, here are the same terms written in Fex:
--
-- > lam (lam (var 2))
-- > lam (lam (lam (app (app (var 3) (var 1)) (app (var 2) (var 1)))))
-- > lam (app (lam (app (var 1) (lam (var 1)))) (lam (app (var 2) (var 1))))
var :: Int -- ^ An integer in the range @[1,)@.
    -> Fex
var = Var . var'
  where var' 0 = error "de Bruijn indexes start at 1"
        var' 1 = VZ
        var' n = VS $ var' (n - 1)

-- | Apply one Fex term to another.
app :: Fex -- ^ Must evaluate to a lambda term.
    -> Fex -- ^ Any value, including a lambda.
    -> Fex
app = App

-- | Introduce a lambda Fex term.
lam :: Fex -- ^ The body of the lambda.
    -> Fex
lam = Lam

-- | Introduce a list of terms as a Fex term.
list :: [Fex] -> Fex
list = List

-- | Read the contents of an environment variable. This introduces a new
-- dependency on the existence of an environment variable.
readEnvVar :: Fex -- ^ The name of the environment variable.
           -> Fex -- ^ A Fex String holding the contents of the variable.
readEnvVar = Dep . ReadEnvVar

-- | Establishes a dependency on a path to an executable and returns a function
-- that accepts a list of arguments and returns the stdout of the process
-- after it terminates. For example, the following will execute the command
-- @ls@ on the directories @\/var@ and @\/usr@:
--
-- > runExe (str "ls") `app` list [str "/var", str "/usr"]
runExe :: Fex -- ^ The name or path to an executable.
       -> Fex -- ^ A function that accepts a list of arguments.
runExe exe = Dep $ RunExe exe

-- | Establishes a dependency on the existence of a /file/. If the file does
-- not exist, then the dependency is not satisfied. Note that this expression
-- is executed for side effect; the same path given is returned unaltered.
file :: Fex -- ^ Path to a file.
     -> Fex -- ^ The same path given, unaltered.
file = Dep . File

-- | Establishes a dependency on the existence of a /directory/. If the
-- directory does not exist, then the dependency is not satisfied. Note that
-- this expression is executed for side effect; the same path given is
-- returned unaltered.
dir :: Fex -- ^ Path to a dir.
    -> Fex -- ^ The same path given, unaltered.
dir = Dep . Dir

-- | Introduce a `Flag` term. `Flag` terms always evaluate to bools, integers,
-- doubles or strings and are guaranteed to be consistent with the supplied
-- default value.
flag :: Flag -> Fex
flag = Flag

