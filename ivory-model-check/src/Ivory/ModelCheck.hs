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
import           Ivory.ModelCheck.Logic2SimpleSMT

-- | Model-check an Ivory definition with the default options
modelCheck' :: [I.Module] -> P.Def p -> IO ()
modelCheck' mods p = do
  res <- modelCheck defaultOpts mods p
  print $ showResult mods p res

-- | Model-check an Ivory definition
modelCheck :: ILOpts -> [I.Module] -> P.Def p -> IO (SMTResult IvoryMemory)
modelCheck opts mods (P.DefProc p) = do
  res <- modelCheckProc defaultZ3Solver opts mods p
  if debugLevel opts >= 1 then
    putStrLn $ showResult mods (P.DefProc p) res
    else return ()
  return res
modelCheck _ _ _ = error "I can only check procedures defined in Ivory!"

-- | Print an 'SMTResult' for the Ivory reachability logic
showResult :: [I.Module] -> P.Def p -> SMTResult IvoryMemory -> String
showResult _ _ SMT_unsat = "No errors reachable!"
showResult _ _ (SMT_unknown msg) = "Error in SMT solver: " ++ msg
showResult mods d (SMT_sat mem) =
  "Errors reachable!\nInput memory:\n" ++
  let args = case d of
        P.DefProc p -> I.procArgs p
        P.DefImport imp -> I.importArgs imp in
  renderStyle style (runM (ppIvoryArgs args) mem mods)

-- | Check whether an 'SMTResult' indicates no bad states are reachable
isSafe :: SMTResult a -> Bool
isSafe SMT_unsat = True
isSafe _ = False

-- | Check whether an 'SMTResult' indicates some bad state is reachable
isUnsafe :: SMTResult a -> Bool
isUnsafe (SMT_sat _) = True
isUnsafe _ = False

-- | Check whether an 'SMTResult' contains an error
isError :: SMTResult a -> Bool
isError (SMT_unknown _) = True
isError _ = False
