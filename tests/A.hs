module Main where

import Debug.Trace (trace)

import Data.List (intercalate)
import Data.Monoid
import qualified System.IO as I

import Development.Fex.Experiment
import Development.Fex.Depends

test :: Experiment String
test = do
  mkExe "cat" >>= runExe
  mkExe "ls" >>= runExe

main = do
  deps <- dependsExper test
  status <- mapM missing deps
  putStrLn $ intercalate "\n" $ zipWith (curry dependStatus) deps status

  putStrLn "\n\nRunning experiment..."
  d <- evalExper test
  case d of
    Left err -> putStrLn err
    Right r -> putStrLn r

  -- deps <- dependsExper test 
  -- print deps 

-- main = do 
  -- d <- evalExper (dep >>= dummy) 
  -- print d 
  -- deps <- dependsExper dummy 
  -- print deps 
--  
  -- d <- evalExper dummy 
  -- print d 

  -- deps <- dependsExper test1 
  -- print deps 
  -- effs <- effectsExper test1 
  -- print effs 
--  
  -- hmm <- evalExper test1 
  -- print hmm 
  -- handle <- evalExper test1 
  -- I.hPutStrLn handle "test test" 
  -- I.hClose handle 

-- test1 :: Experiment () 
-- test1 = do 
  -- depend (DExec "ls", "list files") 
  -- depend (DExec "cat", "cat files") 
  -- experIO $ print "wAt" 
  -- depend (DExec "gzip", "compress files") 
  -- handle <- openFile "./cauchy" I.ReadMode 
  -- s <- experIO $ I.hGetContents handle 
  -- depend (DExec s, "tricky") 
  -- return s 

