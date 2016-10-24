{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

--
-- Type check to ensure there are no empty blocks in procedures, for non-void
-- procedures, a value is returned, there is no dead code (code after a return
-- statement), no field in a struct is initialized twice, and that arrays have
-- the right number of elements initialized.
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

import           Prelude                                 ()
import           Prelude.Compat                          hiding (init)

import           Control.Monad                           (unless, void, when)
import           Data.List                               (nub)
import           MonadLib                                (Id, StateM (..),
                                                          StateT, WriterM (..),
                                                          WriterT, runId,
                                                          runStateT, runWriterT)
import           Text.PrettyPrint

import qualified Ivory.Language.Syntax.AST               as I
import           Ivory.Language.Syntax.Concrete.Location
import qualified Ivory.Language.Syntax.Type              as I
import           Ivory.Opts.Utils

--------------------------------------------------------------------------------
-- Errors types

data RetError = RetError String [Error]
  deriving (Show, Read, Eq)

data Warning = IfTEWarn
  | LoopWarn
  | VoidEmptyBody
  | ArrayInitUnusedWarning
  | ArrayInitPartialWarning
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

data ModuleResult = ModuleResult String Results
  deriving (Show, Read, Eq)

-- | Are there any errors from typechecking?
existErrors :: [ModuleResult] -> Bool
existErrors ls = or (map go ls)
  where
  go (ModuleResult _ res) = not (null (errs res))

showError :: Error -> Doc
showError err = case err of
  EmptyBody  -> text "Procedure contains no statements!"
  NoRet      -> text "No return statement and procedure has a non-void type."
  DeadCode   -> text "Unreachable statements after a return."
  DoubleInit -> text "Repeated initialization of a struct field."

showWarning :: Warning -> Doc
showWarning w = case w of
  IfTEWarn
    ->     text "One branch of an if-then-else statement contains a return statement."
       <+> text "Statements after the if-the-else block are not reachable on all control paths."
  LoopWarn
    -> text "Statements after the loop may be unreachable due to a return statement within the loop."
  VoidEmptyBody
    -> text "Procedure with void return type has no statements."
  ArrayInitPartialWarning
    -> text "Array patially initialized."
  ArrayInitUnusedWarning
    -> text "Array contains unused initializers."

-- | Show all the typechecking errors for a module item.
showErrors :: ModuleResult -> String
showErrors (ModuleResult nm res)
  = mkOut nm "ERROR" (showWithLoc showError) (errs res)

-- | Show all the typechecking warnings for a module item.
showWarnings :: ModuleResult -> String
showWarnings (ModuleResult nm res)
  = mkOut nm "WARNING" (showWithLoc showWarning) (warnings res)

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

-- | Type Check a module.
typeCheck :: I.Module -> [ModuleResult]
typeCheck m = map procTC allProcs ++ map areaTC allAreas
  where
  allProcs = let ps = I.modProcs m in I.public ps ++ I.private ps
  procTC p = ModuleResult (I.procSym p) res
    where
    res = snd $ runTCResults $ tyChk (I.procRetTy p) (I.procBody p)

  allAreas = let as = I.modAreas m in I.public as ++ I.private as
  areaTC a = ModuleResult (I.areaSym a) res
    where
    res = snd $ runTCResults $ checkInit (Just (I.areaType a)) (I.areaInit a)

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
  tyChk' (sb, False) (I.Loop _ _ _ _ ss0 : ss)
    = do b <- tyChk' (True, False) ss0
         when b (putWarn LoopWarn)
         tyChk' (sb, False) ss
  tyChk' b (I.Local t _v init' : ss)
    = do checkInit (Just t) init'
         tyChk' b ss
  tyChk' b (I.Comment (I.SourcePos src):ss)
    = do set src
         tyChk' b ss
  tyChk' b (_:ss)
    = tyChk' b ss

  isComment (I.Comment _) = True
  isComment _             = False

-- XXX We don't check array lengths for substructural arrays (e.g., arrays that
-- are a field of a struct) because we don't have the typing information with
-- the array length in the AST; it's not part of the initializer. We could add it, though.
checkInit :: Maybe I.Type -> I.Init -> TCResults ()
checkInit mty init =
  case init of
    I.InitZero
      -> return ()
    I.InitExpr{}
      -> return ()
    I.InitStruct ls
      -> do
      mapM_ (checkInit Nothing . snd) ls
      let fs = map fst ls
      when (fs /= nub fs) (putError DoubleInit)
    I.InitArray inits b
      -> do
      mapM_ (checkInit Nothing) inits
      unless b (putWarn ArrayInitUnusedWarning)
      let mlen = getArrayLen <$> mty
      case mlen of
        Nothing  -> return ()
        -- Assume default initialization if zero inits
        Just len -> when (not (null inits) && length inits < len)
                         (putWarn ArrayInitPartialWarning)

getArrayLen :: I.Type -> Int
getArrayLen ty = case ty of
  I.TyArr i _
    -> i
  _ -> error $ "Error in TypeChecking: the impossible happened. "
         ++ "Expected an array type but saw " ++ show ty ++ "."

xor :: Bool -> Bool -> Bool
xor a b = (not a && b) || (a && not b)
