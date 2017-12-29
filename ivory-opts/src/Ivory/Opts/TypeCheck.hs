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
  , showTyChkModule
  , existWarnOrErrors
  , existErrors
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

data Warning =
    IfTEWarn
  | LoopWarn
  | VoidEmptyBody
  | ArrayInitUnusedWarning
  | ArrayInitPartialWarning
  deriving (Show, Read, Eq)

data Error =
    EmptyBody
  | NoRet
  | DeadCode
  | DoubleInit
  deriving (Show, Read, Eq)

data Result =
    LocError (Located Error)
  | LocWarn  (Located Warning)
  deriving (Show, Read, Eq)

type Results = [Result]

allResults :: ModResult Result -> [Result]
allResults (ModResult _ ls) = concatMap go ls
  where
  go (SymResult _ res) = res

-- | Are there any errors from typechecking the module?
existErrors :: ModResult Result -> Bool
existErrors m =
  not (null (f (allResults m)))
  where
  f = filter (\a -> case a of LocError{} -> True; LocWarn{} -> False)

-- | Are there any errors or warnings from typechecking the module?
existWarnOrErrors :: ModResult Result -> Bool
existWarnOrErrors m = not (null (allResults m))

showError :: Error -> Doc
showError err =
      text "ERROR"
  <+> text (case err of
              EmptyBody
                -> "Procedure contains no statements!"
              NoRet
                -> "No return statement and procedure has a non-void type."
              DeadCode
                 -> "Unreachable statements after a return."
              DoubleInit
                 -> "Repeated initialization of a struct field."
           )

showWarning :: Warning -> Doc
showWarning w =
      text "WARNING"
  <+> text (case w of
              IfTEWarn
                -> "One branch of an if-then-else statement contains a return statement. Statements after the if-the-else block are not reachable on all control paths."
              LoopWarn
                -> "Statements after the loop may be unreachable due to a return statement within the loop."
              VoidEmptyBody
                -> "Procedure with void return type has no statements."
              ArrayInitPartialWarning
                -> "Array partially initialized."
              ArrayInitUnusedWarning
                -> "Array contains unused initializers."
           )

showTyChkModule
  :: Bool -- ^ show warnings
  -> ModResult Result
  -> Doc
showTyChkModule b res = showModErrs go res
  where
  go :: Result -> Doc
  go (LocError e) = showWithLoc showError e
  go (LocWarn  w) = if b then showWithLoc showWarning w else empty

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
  put [LocError (err `at` loc)]

putWarn :: Warning -> TCResults ()
putWarn warn = do
  loc <- get
  put [LocWarn (warn `at` loc)]

runTCResults :: TCResults a -> (a, Results)
runTCResults tc = fst $ runId $ runStateT NoLoc $ runWriterT (unTC tc)

--------------------------------------------------------------------------------

-- | Type Check a module.
typeCheck :: I.Module -> ModResult Result
typeCheck m =
  ModResult (I.modName m)
            (concatMap procTC allProcs ++ concatMap areaTC allAreas)

  where
  allProcs = let ps = I.modProcs m in I.public ps ++ I.private ps
  procTC p =
    if null res then [] else [SymResult (I.procSym p) res]
    where
    res = snd $ runTCResults $ tyChk (I.procRetTy p) (I.procBody p)

  allAreas = let as = I.modAreas m in I.public as ++ I.private as
  areaTC a =
    if null res then [] else [SymResult (I.areaSym a) res]
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
