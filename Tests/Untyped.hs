module Tests.Untyped where

import Development.FexA.Experiment

-- Examples
id' :: Fex
id' = lam $ var 1

test1 :: Fex
test1 = app id' $ bool True

test2 :: Fex
test2 = app id' $ bool False

testBio :: Fex
testBio = readEnvVar $ str "BIO"

bioLs :: Fex
bioLs = runExe (str "ls") [testBio]

bioLs2 :: Fex
bioLs2 = runExe (readEnvVar $ str "GOROOT") [testBio]

main :: IO ()
main = do
  evalIO' test1 >>= print
  -- evalPrint test2
  -- evalPrint id'
  -- evalPrint testBio
  evalIO' bioLs >>= print
  -- print $ depTree bioLs
  -- print $ depTree bioLs
  -- putStrLn ""
  -- print $ depTree bioLs2
  print $ map showStatic $ staticDeps bioLs2

