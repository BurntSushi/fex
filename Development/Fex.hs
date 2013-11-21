module Development.Fex
  ( module Development.Fex.Depend
  , Experiment, Dependency, Effect, Depend(..)
  , dependsExper, dep, dependStatus, resolve
  , effectsExper, effect
  , evalExper
  , baseExper
  , liftIO
  , experFail
  , setName, getName
  , cached, existsAfter
  )
where

import Development.Fex.Base
import Development.Fex.Combinators
import Development.Fex.Depend
import Development.Fex.Experiment

