module Development.Fex.Experiment
where

newtype Experiment a = Experiment { runExperiment :: Exper -> (a, Exper) }

data Exper = Exper { depends :: [Dependency]
                   , effects :: [Effect]
                   }

type Dependency = (Dep, String)

data Dep
  = DFlag { fshort :: Char, flong :: String, fdefault :: String }
  | DEnv { ename :: String, edefault :: String }
  | DExec String
  | DFile String
  | DDir String

type Effect = (Eff, String)

data Eff
  = EFile String

