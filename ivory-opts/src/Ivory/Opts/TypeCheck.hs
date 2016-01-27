{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

--
-- Type check to ensure there are no empty blocks in procedures, for non-void
-- procedures, a value is returned, there is no dead code (code after a return
-- statement), no field in a struct is initialized twice.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Opts.TypeCheck
  ( typeCheck
  , showErrors
  , showWarnings
  , existErrors
  , Results()
  ) where

import Prelude ()
import Prelude.Compat

import Control.Monad (when,void)
import MonadLib
           (WriterM(..),StateM(..),runId,runStateT,runWriterT,Id,StateT,WriterT)
import Data.List (nub)

import Ivory.Language.Syntax.Concrete.Location
import Ivory.Language.Syntax.Concrete.Pretty
import qualified Ivory.Language.Syntax.AST  as I
import qualified Ivory.Language.Syntax.Type as I

--------------------------------------------------------------------------------
-- Errors types

data RetError = RetError String [Error]
  deriving (Show, Read, Eq)

data Warning = IfTEWarn
             | LoopWarn
             | VoidEmptyBody
  deriving (Show, Read, Eq)

data Error = EmptyBody
           | NoRet
           | DeadCode
           | DoubleInit
  deriving (Show, Read, Eq)

data Results = Results
  { errs     :: [Located Error]
  , warnings :: [Located Warning]
  } deriving (Show, Read, Eq)

instance Monoid Results where
  mempty = Results [] []
  Results a0 b0 `mappend` Results a1 b1 = Results (a0 ++ a1) (b0 ++ b1)

-- | Are there any errors from typechecking?
existErrors :: Results -> Bool
existErrors = not . null . errs

showError :: Error -> String
showError err = case err of
  EmptyBody  -> "Procedure contains no statements!"
  NoRet      -> "No return statment and procedure has a non-void type."
  DeadCode   -> "Unreachable statements after a return."
  DoubleInit -> "Repeated initialization of a struct field."

showWarning :: Warning -> String
showWarning w = case w of
  IfTEWarn
    -> "One branch of an if-then-else statement contains a return statement.\nStatements after the if-the-else block are not reachable on all control paths."
  LoopWarn
    -> "Statements after the loop may be unreachable due to a return statement within the loop."
  VoidEmptyBody
    -> "Procedure with void return type has no statements."

showWithLoc :: (a -> String) -> Located a -> String
showWithLoc sh (Located loc a) = prettyPrint (pretty loc) ++ ": " ++ sh a

-- | Given a procedure name, show all the typechecking results for that procedure.
showErrors :: String -> Results -> [String]
showErrors procName res
  = mkOut procName "ERROR" (showWithLoc showError) (errs res)

-- | Given a procedure name, show all the typechecking results for that procedure.
showWarnings :: String -> Results -> [String]
showWarnings procName res
  = mkOut procName "WARNING" (showWithLoc showWarning) (warnings res)

mkOut :: String -> String -> (a -> String) -> [a] -> [String]
mkOut _   _    _  [] = []
mkOut sym kind sh ls = nm : map go ls
  where
  go x = "   " ++ kind ++ ": " ++ sh x
  nm   = "*** Procedure " ++ sym

--------------------------------------------------------------------------------
-- Writer Monad

newtype TCResults a = TCResults { unTC :: WriterT Results (StateT SrcLoc Id) a }
  deriving (Functor, Applicative, Monad)

instance WriterM TCResults Results where
  put e = TCResults (put e)

instance StateM TCResults SrcLoc where
  get = TCResults get
  set = TCResults . set

putError :: Error -> TCResults ()
putError err = do
  loc <- get
  put (Results [err `at` loc] [])

putWarn :: Warning -> TCResults ()
putWarn warn = do
  loc <- get
  put (Results [] [warn `at` loc])

runTCResults :: TCResults a -> (a, Results)
runTCResults tc = fst $ runId $ runStateT NoLoc $ runWriterT (unTC tc)

--------------------------------------------------------------------------------

-- | Type Check a procedure.
typeCheck :: I.Proc -> Results
typeCheck p = snd $ runTCResults $ tyChk (I.procRetTy p) (I.procBody p)

-- Sub-block of the prcedure
type SubBlk = Bool
-- Seen a return statement?
type Ret = Bool

tyChk :: I.Type -> [I.Stmt] -> TCResults ()
tyChk I.TyVoid  []    = putWarn VoidEmptyBody
tyChk _         []    = putError EmptyBody
tyChk ty        stmts = void (tyChk' (False, False) stmts)
  where
  tyChk' :: (SubBlk, Ret) -> [I.Stmt] -> TCResults Ret
  -- Ret and no other statemnts
  tyChk' (_, True) ss | all isComment ss
    = return True
  -- Ret and other statements
  tyChk' (sb, True) ss
    = putError DeadCode >> tyChk' (sb, False) ss
  -- Sub block and no ret seen
  tyChk' (True, False) []
    = return False
  -- No ret seen, main block: only a problem if non-void type.
  tyChk' (False, False) []
    = do when (ty /= I.TyVoid) (putError NoRet)
         return False
  -- The two return cases
  tyChk' (sb, False) (I.ReturnVoid : ss)
    = tyChk' (sb, True) ss
  tyChk' (sb, False) (I.Return _ : ss)
    = tyChk' (sb, True) ss
  -- Control flow
  tyChk' (sb, False) (I.IfTE _ ss0 ss1 : ss)
    = do b0 <- tyChk' (True, False) ss0
         b1 <- tyChk' (True, False) ss1
         if b0 && b1 then tyChk' (sb, True) ss
           else do when (b0 `xor` b1) (putWarn IfTEWarn)
                   tyChk' (sb, False) ss
  tyChk' (sb, False) (I.Loop _ _ _ ss0 : ss)
    = do b <- tyChk' (True, False) ss0
         when b (putWarn LoopWarn)
         tyChk' (sb, False) ss
  tyChk' b (I.Local _t _v init' : ss)
    = do checkInit init'
         tyChk' b ss
  tyChk' b (I.Comment (I.SourcePos src):ss)
    = do set src
         tyChk' b ss
  tyChk' b (_:ss)
    = tyChk' b ss

  isComment (I.Comment _) = True
  isComment _             = False

  checkInit (I.InitStruct fis)
    = do mapM_ (checkInit . snd) fis
         let fs = map fst fis
         when (fs /= nub fs) $
           putError DoubleInit
         return ()
  checkInit (I.InitArray is)
    = mapM_ checkInit is
  checkInit _
    = return ()

xor :: Bool -> Bool -> Bool
xor a b = (not a && b) || (a && not b)
