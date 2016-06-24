{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE CPP #-}

module Ivory.ModelCheck where

import Prelude ()
import Prelude.Compat
import MonadLib
import Text.PrettyPrint

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
  printResult mods p res

-- | Model-check an Ivory definition
modelCheck :: ILOpts -> [I.Module] -> P.Def p -> IO (SMTResult IvoryMemory)
modelCheck opts mods (P.DefProc p) = do
  modelCheckProc defaultZ3Solver opts mods p
modelCheck _ _ _ = error "I can only check procedures defined in Ivory!"

-- | Print an 'SMTResult' for the Ivory reachability logic
printResult :: [I.Module] -> P.Def p -> SMTResult IvoryMemory -> IO ()
printResult _ _ SMT_unsat = print "No errors reachable!"
printResult _ _ (SMT_unknown msg) = print $ "Error in SMT solver: " ++ msg
printResult mods d (SMT_sat mem) = do
  print "Errors reachable!"
  print "Input memory:"
  let args = case d of
        P.DefProc p -> I.procArgs p
        P.DefImport imp -> I.importArgs imp
  print $ renderStyle style (runM (ppIvoryArgs args) mem mods)
