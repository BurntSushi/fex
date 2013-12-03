{-# LANGUAGE FlexibleContexts, FlexibleInstances, Rank2Types, TypeSynonymInstances #-}
module Development.FexA.Experiment
  (

  -- * The Fex language
    Fex
  , Value
  , bool, int, double, str
  , var, app, lam

  -- * Evaluating a Fex term
  , EvalContext
  , Pure
  , evalIO

  -- * Dependencies
  , Dependency
  , readEnvVar, runExe

  -- * Dependency analysis
  , DepTree, depTree, staticDeps, showStatic
  )
where

import Control.Monad.Identity (Identity, runIdentity)
import Data.List (intercalate)
import System.Environment (getEnv)
import System.Process (readProcess)
import Text.Printf (PrintfArg, printf)

data Fex m = Lit (RawValue m)
            | Var Var
            | Dep (Dependency m)
            | App (Fex m) (Fex m)
            | Lam (Fex m)
            deriving Show

data Var = VZ | VS Var deriving Show

type Env m = [RawValue m]

data RawValue m = Bool Bool
                | Int Int
                | Double Double
                | String String
                | Closure (RawValue m -> m (RawValue m))

type Pure = Identity

type Value = RawValue IO

instance Show (RawValue m) where
  show (Bool b) = show b
  show (Int n) = show n
  show (Double d) = show d
  show (String s) = s
  show (Closure _) = "<closure>"

data Dependency m = ReadEnvVar (Fex m)
                  | RunExe (Fex m) [Fex m]
                  deriving Show

class Monad m => EvalContext m where
  -- | Evaluates a dependency in a particular context.
  deval :: Dependency m -- ^ The dependency to evaluate.
        -> (Fex m -> m (RawValue m)) -- ^ An evaluation environment.
        -> m (RawValue m) -- ^ The result of evaluating a dependency.

instance EvalContext Pure where
  deval d _ = errf "Dependency can't be evaluated in pure context: %s" (show d)

instance EvalContext IO where
  deval (ReadEnvVar e) ev = ev e >>= \e -> fmap String $ getEnv $ valueToStr e
  deval (RunExe exe args) ev = do
    exe' <- fmap valueToStr $ ev exe
    args' <- fmap (map valueToStr) $ mapM ev args
    fmap String $ readProcess exe' args' ""

-- Evaluation
lookv :: EvalContext m => Var -> Env m -> Maybe (RawValue m)
lookv _ [] = Nothing
lookv VZ (x:_) = Just x
lookv (VS v) (_:xs) = lookv v xs

evalIO :: Fex IO -> IO Value
evalIO = eval

evalPure :: Fex Pure -> RawValue Pure
evalPure = runIdentity . eval

eval :: EvalContext m => Fex m -> m (RawValue m)
eval = eval' []

eval' :: EvalContext m => Env m -> Fex m -> m (RawValue m)
eval' env = ev
  where ev (Lit v) = return v
        ev (Var v) = case lookv v env of
                       Nothing  -> errf "Unbound variable '%s'" (show v)
                       Just val -> return val
        ev (Dep d) = deval d ev
        ev (App f v) = do
          f' <- ev f
          v' <- ev v
          case f' of
            Closure f'' -> f'' v'
            _ -> errf "Non-function value in LHS of app: %s" (show f')
        ev (Lam body) = return $ Closure $ \v -> eval' (v:env) body

valueToStr :: RawValue m -> String
valueToStr v = case v of
                 String s -> s
                 _ -> errf "Expected String; got %s" (show v)

errf :: PrintfArg r => String -> r -> a
errf s r = error $ printf s r


-- Dependency analysis
data DepTree = Node (Maybe (Dependency Pure)) [DepTree]

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

depTidy :: DepTree -> DepTree
depTidy (Node d ts) = Node d $ filter (not . isNil) $ map depTidy ts
  where isNil (Node Nothing []) = True
        isNil _ = False

depTree :: Fex Pure -> DepTree
depTree = depTidy . depTree'
  where depTree' :: Fex Pure -> DepTree
        depTree' (Lit _) = depLeaf
        depTree' (Var _) = depLeaf
        depTree' (App f v) = depTree' f `depJoin` depTree' v
        depTree' (Lam body) = depTree' body
        depTree' (Dep d) = dt d

        dt :: Dependency Pure -> DepTree
        dt d@(ReadEnvVar e) = Node (Just d) [depTree' e]
        dt d@(RunExe exe args) = foldl depJoin exeDep argDeps
          where exeDep = Node (Just d) [depTree' exe]
                argDeps = map depTree' args

staticDeps :: Fex Pure -> [Dependency Pure]
staticDeps = sdeps . depTree
  where sdeps (Node (Just d) []) = [d]
        sdeps (Node _ ts) = concatMap sdeps ts

showDynamic :: Dependency Pure -> String
showDynamic (ReadEnvVar _) = "ReadEnvVar"
showDynamic (RunExe _ _) = "RunExe"

showStatic :: Dependency Pure -> String
showStatic (ReadEnvVar e) = printf "ReadEnvVar '%s'" $ show $ evalPure e
showStatic (RunExe exe _) = printf "RunExe '%s'" $ show $ evalPure exe

-- Combinators for constructing terms in the Fex language

bool :: Bool -> Fex m
bool = Lit . Bool

int :: Int -> Fex m
int = Lit . Int

double :: Double -> Fex m
double = Lit . Double

str :: String -> Fex m
str = Lit . String

var :: Int -> Fex m
var = Var . var'
  where var' 0 = VZ
        var' n = VS $ var' (n - 1)

app :: Fex m -> Fex m -> Fex m
app = App

lam :: Fex m -> Fex m
lam = Lam

readEnvVar :: Fex m -> Fex m
readEnvVar = Dep . ReadEnvVar

runExe :: Fex m -> [Fex m] -> Fex m
runExe exe args = Dep $ RunExe exe args

