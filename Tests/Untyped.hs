module Tests.Untyped where

import Development.FexA.Experiment

-- Examples
id' :: Fex m
id' = lam $ var 0

test1 :: Fex m
test1 = app id' $ bool True

test2 :: Fex m
test2 = app id' $ bool False

testBio :: Fex m
testBio = readEnvVar $ str "BIO"

bioLs :: Fex m
bioLs = runExe (str "ls") [testBio]

bioLs2 :: Fex m
bioLs2 = runExe (readEnvVar $ str "GOROOT") [testBio]

main :: IO ()
main = do
  evalIO test1 >>= print
  -- evalPrint test2
  -- evalPrint id'
  -- evalPrint testBio
  -- evalPrint bioLs
  -- print $ depTree bioLs
  -- print $ depTree bioLs
  -- putStrLn ""
  -- print $ depTree bioLs2
  print $ map showStatic $ staticDeps bioLs2

