{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE CPP #-}

module Ivory.ModelCheck where

import Prelude ()
import Prelude.Compat

import qualified Ivory.Language.Proc         as P
import qualified Ivory.Language.Syntax       as I
import           Ivory.Language.Syntax.Concrete.Location
import           Ivory.ModelCheck.Logic
import           Ivory.ModelCheck.Ivory2Logic
import           Ivory.ModelCheck.Logic2Z3

-- | Model-check an Ivory definition with the default options
modelCheck' :: [I.Module] -> P.Def p -> IO ()
modelCheck' mods p = do
  res <- modelCheck defaultOpts mods p
  printResult p res

-- | Model-check an Ivory definition
modelCheck :: ILOpts -> [I.Module] -> P.Def p -> IO (SMTResult IvoryMemory)
modelCheck opts mods (P.DefProc p) = do
  modelCheckProc Z3Solver opts mods p
modelCheck _ _ _ = error "I can only check procedures defined in Ivory!"

-- | Print an 'SMTResult' for the Ivory reachability logic
printResult :: P.Def p -> SMTResult IvoryMemory -> IO ()
printResult _ SMT_unsat = print "No errors reachable!"
printResult _ (SMT_unknown msg) = print $ "Error in SMT solver: " ++ msg
printResult _ (SMT_sat mem) = print "Errors reachable!"
-- FIXME HERE NOW: print the initial error state!
