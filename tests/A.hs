module Main where

import Debug.Trace (trace)

import Data.Monoid
import qualified System.IO as I

import Development.Fex.Experiment
import Development.Fex.IO

test1 :: Experiment String
test1 = do
  depend (DExec "ls", "list files")
  depend (DExec "cat", "cat files")
  dummy

main = do
  deps <- dependsExper dummy
  print deps

  d <- evalExper dummy
  print d

  -- handle <- evalExper test1 
  -- contents <- I.hGetContents handle 
  -- print contents 

