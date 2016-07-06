{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

--
-- Quasiquote Ivory areas.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.Syntax.Concrete.QQ.AreaQQ
  ( fromArea
  , fromAreaImport
  ) where

import           Language.Haskell.TH                      hiding (Exp, Stmt,
                                                           Type)

import           Ivory.Language.Syntax.Concrete.ParseAST
import           Ivory.Language.Syntax.Concrete.QQ.Common
import           Ivory.Language.Syntax.Concrete.QQ.ExprQQ
import           Ivory.Language.Syntax.Concrete.QQ.TypeQQ

import qualified Ivory.Language.Init                      as I
import qualified Ivory.Language.MemArea                   as I

-- val  = area "value" (Just (istruct [field .= ival 0]))
fromArea :: AreaDef -> Q [Dec]
fromArea a = do
  (t, _) <- runToQ (fromType (areaType a))
  let ty = AppT (ConT (if c then ''I.ConstMemArea else ''I.MemArea)) t
  return [SigD (mkName nm) ty, d]

  where
  c         = areaConst a
  nm        = allocRefVar (areaInit a)
  d         = ValD (VarP $ mkName nm) (NormalB imp) []
  cntr      = VarE (if c then 'I.constArea else 'I.area)
  imp       = AppE (AppE cntr (LitE (StringL nm))) conIns
  conIns    = if c then ins else AppE (ConE 'Just) ins
  ins       =
    case areaInit a of
      AllocBase   _ mi
        -> case mi of
             Nothing -> VarE 'I.izero
             Just i  -> AppE (VarE 'I.ival) (toExp [] i)
      AllocArr    _ i
        -> case i of
             [] -> VarE 'I.izero
             es -> let mkIval = AppE (VarE 'I.ival) in
                   let is     = map (toExp []) es in
                   let lis    = ListE (map mkIval is) in
                   AppE (VarE 'I.iarray) lis
      AllocStruct _ i
        -> case i of
             Empty
               -> AppE (VarE 'I.istruct) (ListE [])
             MacroInit (fn,args)
               -> let es = map (toExp []) args in
                  callit (mkVar fn) es
             FieldInits fieldAssigns
               -> let es = map (toExp [] . snd) fieldAssigns in
                  let ls = ListE $ map assign (zip (fst $ unzip fieldAssigns) es) in
                  AppE (VarE 'I.istruct) ls
               where
               assign (fnm, e) = InfixE (Just $ mkVar fnm) (VarE '(I..=)) (Just $ mkIval e)
               mkIval = AppE (VarE 'I.ival)

fromAreaImport :: AreaImportDef -> Q [Dec]
fromAreaImport a = do
  (t, _) <- runToQ (fromType (aiType a))
  let ty = AppT (ConT (if c then ''I.ConstMemArea else ''I.MemArea)) t
  return [SigD (mkName nm) ty, d]

  where
  c     = aiConst a
  nm    = aiSym a
  d     = ValD (VarP $ mkName nm) (NormalB imp) []
  cntr  = VarE (if c then 'I.importConstArea else 'I.importArea)
  imp   = AppE (AppE cntr (LitE (StringL nm))) (LitE $ StringL (aiFile a))
