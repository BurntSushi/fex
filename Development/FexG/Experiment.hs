module Development.FexG.Experiment
where

import Control.Monad (liftM2)
import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)
import Text.Printf (printf)

-- The base language for writing experiments. Strings, lambdas and application.
class Base repr where
  str :: String -> repr String
  lam :: (repr a -> repr b) -> repr (a -> b)
  app :: repr (a -> b) -> repr a -> repr b

-- The primary evaluator. It forces every computation to be in the IO
-- monad. Dependency analysis must be run before calling eval.
newtype Eval a = E { unE :: IO a }

instance Base Eval where
  str = E . return
  lam f = E $ return $ \a -> unsafePerformIO $ unE $ f $ E $ return a
  -- In order to write `lam` without `unsafePerformIO`, it appears we need
  -- a function with type `(m a -> m b) -> m (a -> b)`, which Hoogle says
  -- does not exist. I could not imagine one either.
  app f a = E $ do
    f' <- unE f
    a' <- unE a
    return $ f' a'

-- A pretty printer evaluator. Just for demoing.
-- This shows how we can "inspect" the body of a lambda even though it's
-- a Haskell lambda.
newtype PPrint a = P { unP :: Int -> String } -- VarCounter -> String

instance Base PPrint where
  str s = P $ const s
  lam f = P $ \c -> let x = printf "x%d" c
                     in printf "(\\%s -> %s)" x (unP (f (P $ const x)) (c+1))
  app f a = P $ \c -> printf "%s %s" (unP f c) (unP a c)

-- A representation for a dependency. Right now, it's just a name.
data Dependency = Dependency { name :: String } deriving Show

-- A dependency evaluator. Traverses abstract syntax and accumulates all
-- dependencies.
-- N.B. This might be better as a tree structure.
newtype Depends a = D { unD :: [Dependency] }

-- No dependencies in the base language.
instance Base Depends where
  str _ = D []
  lam f = D $ unD $ f $ D []
  app f a = D $ unD f ++ unD a

-- Shortcuts for our evaluators.
eval :: Eval a -> IO a
eval = unE

pprint :: PPrint a -> String
pprint t = unP t 0

depends :: Depends a -> [Dependency]
depends = unD

---------------------

-- Describe an environment variable dependency.
class Base repr => EnvVar repr where
  -- Note here that the type could also be:
  -- readEnvVar :: repr String -> repr (IO String)
  readEnvVar :: repr String -> repr String

instance EnvVar Eval where
  readEnvVar s = E $ unE s >>= \s' -> getEnv s' -- remember Eval is in IO monad

instance EnvVar PPrint where
  readEnvVar s = P $ \c -> printf "(ENV '%s')" (unP s c)

instance EnvVar Depends where
  readEnvVar s = D $ Dependency "ENV VAR" : unD s
  -- Ahhhh! How do we get the name of the environment variable?
  -- It's inside `s`, but evaluating it *may* require IO.
  -- We could add a check to see if there is any IO required in `s`, but this
  -- results in more pain for the person writing dependencies. A lot more pain.
  --
  -- But it is interesting: the very non-existence of IO in `s` *implies*
  -- that this environment variable is a static dependency. Otherwise, it is
  -- necessarily a dynamic dependency.

-- Describes a executable dependency.
-- Note that it only depends on the existence of the executable and not on
-- any properties of the argument list.
class Base repr => Exe repr where
  runExe :: repr String -> [repr String] -> repr String

instance Exe Eval where
  runExe cmd args = E $ do
    cmd' <- unE cmd
    args' <- mapM unE args
    readProcess cmd' args' ""

instance Exe PPrint where
  runExe cmd args = P $ \c -> let args' = unwords $ map (`unP` c) args
                               in printf "(EXE `%s %s`)" (unP cmd c) args'

instance Exe Depends where
  -- runExe should *only* be a static dependency when `cmd` can be determined
  -- without performing any IO. But that condition is not checked here.
  runExe cmd args = D $ Dependency "Executable" : (unD cmd ++ concatMap unD args)

---------------------

-- A simple test: read and return the contents of an env var.
testBio :: EnvVar repr => repr String
testBio = readEnvVar $ str "BIO"

-- Example of compounding dependencies.
-- Depends on the `ls` executable and uses the contents of an env var to
-- pass as an argument.
--
-- Given my mental model of how static dependencies work, this syntax *should*
-- report both an executable dependency and an environment variable dependency.
-- Namely, both can be determined without IO.
--
-- This relies on an interesting property of an executable dependency: it is
-- only dependent on the existence of an executable name. This means that the
-- arguments passed to `ls` need not be static. This property is preserved
-- by the Exe implementation of the Depend type class.
--
-- Things to think about: if the type of `readEnvVar` was
-- `repr String -> repr (IO String)`, then could `bioLs` still be written? How?
bioLs :: (EnvVar repr, Exe repr) => repr String
bioLs = runExe (str "ls") [readEnvVar $ str "BIO"]

-- output some examples.
main :: IO ()
main = do
  putStrLn $ pprint testBio
  eval testBio >>= putStrLn
  print $ pprint bioLs
  eval bioLs >>= putStrLn
  print (depends bioLs)

