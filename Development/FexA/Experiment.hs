module Development.FexA.Experiment
where

import Data.List (intercalate)
import System.Environment (getEnv)
import System.Process (readProcess)
import Text.Printf (PrintfArg, printf)

data Expr = Lit Value
          | Var Var
          | Dep Dependency
          | App Expr Expr
          | Lam Expr
          deriving Show

data Var = VZ | VS Var deriving Show

type Env = [Value]

data Value = Bool Bool
           | Int Int
           | Double Double
           | String String
           | Closure (Value -> IO Value)

instance Show Value where
  show (Bool b) = show b
  show (Int n) = show n
  show (Double d) = show d
  show (String s) = s
  show (Closure _) = "<closure>"

data Dependency = ReadEnvVar Expr
                | RunExe { exe :: Expr, args :: [Expr] }
                deriving Show

-- Evaluation
lookv :: Var -> Env -> Maybe Value
lookv _ [] = Nothing
lookv VZ (x:_) = Just x
lookv (VS v) (_:xs) = lookv v xs

evalPrint :: Expr -> IO ()
evalPrint e = eval e >>= print

eval :: Expr -> IO Value
eval = eval' []

eval' :: Env -> Expr -> IO Value
eval' env = ev
  where ev :: Expr -> IO Value
        ev (Lit v) = return v
        ev (Var v) = case lookv v env of
                      Nothing  -> errf "Unbound variable '%s'" (show v)
                      Just val -> return val
        ev (Dep d) = dep d
        ev (App f v) = do
          f' <- ev f
          v' <- ev v
          case f' of
            Closure f'' -> f'' v'
            _ -> errf "Non-function value in LHS of app: %s" (show f')
        ev (Lam body) = return $ Closure $ \v -> eval' (v:env) body

        dep :: Dependency -> IO Value
        dep (ReadEnvVar e) = ev e >>= \e -> fmap String $ getEnv $ valueToStr e
        dep (RunExe exe args) = do
          exe' <- fmap valueToStr $ ev exe
          args' <- fmap (map valueToStr) $ mapM ev args
          fmap String $ readProcess exe' args' ""

valueToStr :: Value -> String
valueToStr v = case v of
                 String s -> s
                 _ -> errf "Expected String; got %s" (show v)

errf :: PrintfArg r => String -> r -> a
errf s r = error $ printf s r


-- Dependency analysis
data DepTree = Node (Maybe Dependency) [DepTree]

depLeaf :: DepTree
depLeaf = Node Nothing []

depJoin :: [DepTree] -> DepTree
depJoin = Node Nothing

instance Show DepTree where
  show = show' 0
    where show' :: Int -> DepTree -> String
          show' _ (Node Nothing []) = ""
          show' depth (Node Nothing ts) =
            printf "%s%s" (dstr depth) (subdeps depth ts)
          show' depth (Node (Just d) []) =
            printf "%s%s" (dstr depth) (show d)
          show' depth (Node (Just d) ts) =
            printf "%s%s\n%s%s" (dstr depth) (show d)
                                (dstr depth) (subdeps (depth+1) ts)
          
          dstr :: Int -> String
          dstr n = concat $ replicate n "  "

          subdeps :: Int -> [DepTree] -> String
          subdeps depth = intercalate "\n" . map (show' depth)

depTidy :: DepTree -> DepTree
depTidy (Node d ts) = Node d $ filter (not . isNil) $ map depTidy ts
  where isNil (Node Nothing []) = True
        isNil _ = False

depTree :: Expr -> DepTree
depTree = depTidy . depTree' allDeps

depTree' :: (Dependency -> DepTree) -> Expr -> DepTree
depTree' _ (Lit _) = depLeaf
depTree' _ (Var _) = depLeaf
depTree' dt (App f v) = depJoin [depTree' dt f, depTree' dt v]
depTree' dt (Lam body) = depTree' dt body
depTree' dt (Dep d) = dt d

allDeps :: Dependency -> DepTree
allDeps d@(ReadEnvVar e) = Node (Just d) [depTree' allDeps e]
allDeps d@(RunExe exe args) = depJoin $ Node (Just d) [exeDep]:argDeps
  where exeDep = depTree' allDeps exe
        argDeps = map (depTree' allDeps) args

staticDeps :: Expr -> [Dependency]
staticDeps = sdeps . depTree
  where sdeps (Node (Just d) []) = [d]
        sdeps (Node _ ts) = concatMap sdeps ts

-- Examples
id' :: Expr
id' = Lam (Var VZ)

test1 :: Expr
test1 = App id' (Lit $ Bool True)

test2 :: Expr
test2 = App id' (Lit $ Bool False)

readEnv :: String -> Expr
readEnv = Dep . ReadEnvVar . Lit . String

testBio :: Expr
testBio = readEnv "BIO"

bioLs :: Expr
bioLs = Dep $ RunExe (Lit $ String "ls") [readEnv "BIO"]

bioLs2 :: Expr
bioLs2 = Dep $ RunExe (readEnv "GOROOT") [testBio]

main :: IO ()
main = do
  evalPrint test1
  evalPrint test2
  evalPrint id'
  evalPrint testBio
  evalPrint bioLs
  print $ depTree bioLs
  print $ staticDeps bioLs
  print $ staticDeps bioLs2

