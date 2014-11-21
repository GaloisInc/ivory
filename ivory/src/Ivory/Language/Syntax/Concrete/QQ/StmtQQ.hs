{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

--
-- QuasiQuoter for Ivory statements.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.Syntax.Concrete.QQ.StmtQQ
 ( fromProgram
 ) where

import           Prelude hiding (exp, init)

import           Language.Haskell.TH        hiding (Stmt, Exp, Type)
import qualified Language.Haskell.TH   as T

import qualified Ivory.Language.Init   as I
import qualified Ivory.Language.Ref    as I
import qualified Ivory.Language.Proc   as I
import qualified Ivory.Language.Assert as I
import qualified Ivory.Language.IBool  as I
import qualified Ivory.Language.Loop   as I
import qualified Ivory.Language.Monad  as I

import           Control.Monad (forM_)

import Ivory.Language.Syntax.Concrete.Location
import Ivory.Language.Syntax.Concrete.ParseAST
import Ivory.Language.Syntax.Concrete.QQ.BindExp
import Ivory.Language.Syntax.Concrete.QQ.Common
import Ivory.Language.Syntax.Concrete.QQ.TypeQQ

--------------------------------------------------------------------------------

fromProgram :: [Stmt] -> Q T.Exp
fromProgram program =
  if null program then [| return () |]
    else  return . DoE =<< (runToSt $ forM_ program fromStmt)

fromBlock :: [Stmt] -> TStmtM T.Exp
fromBlock = liftQ . fromProgram

fromStmt :: Stmt -> TStmtM ()
fromStmt stmt = case stmt of
  IfTE cond blk0 blk1
    -> do
    cd <- fromExpStmt cond
    b0 <- fromBlock blk0
    b1 <- fromBlock blk1
    insert $ NoBindS (AppE (AppE (AppE (VarE 'I.ifte_) cd) b0) b1)
  Assert exp
    -> do
    e <- fromExpStmt exp
    insert $ NoBindS (AppE (VarE 'I.assert) e)
  Assume exp
    -> do
    e <- fromExpStmt exp
    insert $ NoBindS (AppE (VarE 'I.assume) e)
  Return exp
    -> do
    e <- fromExpStmt exp
    insert $ NoBindS (AppE (VarE 'I.ret) e)
  ReturnVoid
    -> insert $ NoBindS (VarE 'I.retVoid)
  Store exp0 exp1
    -> do
    a <- fromAreaStmt (expToArea exp0)
    e <- fromExpStmt exp1
    let storeIt p = insert $ NoBindS (AppE (AppE (VarE 'I.store) p) e)
    storeIt a
  Assign var exp mtype
    -> do
    e <- fromExpStmt exp
    let v = mkName var
    eTy <- case mtype of
             Nothing -> return e
             Just ty -> do tyQ <- liftQ $ runToQ (fromType ty)
                           return (SigE e (fst tyQ))
    insert $ BindS (VarP v) (AppE (VarE 'I.assign) eTy)
  NoBindCall sym args
    -> do
    es <- fromArgs args
    let call f = AppE (VarE f) (mkVar sym)
    insert $ NoBindS (callit (call 'I.call_) es)
  RefCopy refDest refSrc
    -> do
    eDest <- fromExpStmt refDest
    eSrc  <- fromExpStmt refSrc
    insert $ NoBindS (AppE (AppE (VarE 'I.refCopy) eDest) eSrc)
  Forever blk
    -> do
    b <- fromBlock blk
    insert $ NoBindS (AppE (VarE 'I.forever) b)
  Break
    -> insert $ NoBindS (VarE 'I.breakOut)
  AllocRef alloc
    -> fromAlloc alloc
  MapArr ixVar blk
    -> do
    b <- fromBlock blk
    insert $ NoBindS (AppE (VarE 'I.arrayMap) (LamE [VarP (mkName ixVar)] b))
  UpTo exp ixVar blk
    -> do
    e <- fromExpStmt exp
    b <- fromBlock blk
    insert $ NoBindS (AppE (AppE (VarE 'I.for) e) (LamE [VarP (mkName ixVar)] b))
  -- Either a single variable or a function call.
  IvoryMacroStmt mv (nm,args)
    -> do es <- fromArgs args
          let c = callit (mkVar nm) es
          insert $ case mv of
                     Nothing -> NoBindS c
                     Just v  -> BindS (VarP $ mkName v) c
  LocStmt s
    -> fromStmt (unLoc s)

--------------------------------------------------------------------------------
-- Initializers

fromAlloc :: AllocRef -> TStmtM ()
fromAlloc alloc = case alloc of
  AllocBase ref mexp
    -> do e <- case mexp of
                 Nothing  -> return (VarE 'I.izero)
                 Just exp -> fromExpStmt exp
          let p = mkName ref
          insert $ BindS (VarP p)
                         (AppE (VarE 'I.local) (AppE (VarE 'I.ival) e))
  AllocArr arr exps
    -> do es <- mapM fromExpStmt exps
          let mkIval = AppE (VarE 'I.ival)
          let init = ListE (map mkIval es)
          let p = mkName arr
          insert $ BindS (VarP p)
                         (AppE (VarE 'I.local) (AppE (VarE 'I.iarray) init))
  AllocStruct s init
    -> do i <- fromInit
          insert $ BindS (VarP p)
                         (AppE (VarE 'I.local) i)
    where
    p = mkName s
    fromInit = case init of
      Empty
        -> liftQ $ appE (varE 'I.istruct) [|[]|]
      MacroInit (nm,args)
        -> do es <- fromArgs args
              return (callit (mkVar nm) es)
      FieldInits fieldAssigns
        -> do es <- mapM (fromExpStmt . snd) fieldAssigns
              let ls = ListE $ map assign (zip (fst $ unzip fieldAssigns) es)
              return (AppE (VarE 'I.istruct) ls)
        where
        assign (fnm, e) = InfixE (Just $ mkVar fnm) (VarE '(I..=)) (Just $ mkIval e)
        mkIval = AppE (VarE 'I.ival)

--------------------------------------------------------------------------------

fromArgs :: [Exp] -> QStM T.Stmt [T.Exp]
fromArgs = mapM fromExpStmt
