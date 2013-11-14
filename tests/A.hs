module Main where

import Debug.Trace (trace)

import Data.Monoid
import qualified System.IO as I

import Development.Fex.Experiment
import Development.Fex.IO

test1 :: Experiment ()
test1 = do
  depend (DExec "ls", "list files")
  depend (DExec "cat", "cat files")
  experIO $ print "wAt"
  depend (DExec "gzip", "compress files")
  -- handle <- openFile "./cauchy" I.ReadMode 
  -- s <- experIO $ I.hGetContents handle 
  -- depend (DExec s, "tricky") 
  -- return s 

main = do
  -- deps <- dependsExper (dummy >> dummy) 
  -- print deps 
--  
  -- d <- evalExper dummy 
  -- print d 

  deps <- dependsExper test1
  print deps
  effs <- effectsExper test1
  print effs

  hmm <- evalExper test1
  print hmm
  -- handle <- evalExper test1 
  -- I.hPutStrLn handle "test test" 
  -- I.hClose handle 
