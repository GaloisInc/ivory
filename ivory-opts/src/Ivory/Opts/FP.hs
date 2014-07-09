{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

--------------------------------------------------------------------------------
-- | Assert folding: add asserts expression NaN and Infinite floats.
--------------------------------------------------------------------------------

module Ivory.Opts.FP
  ( fpFold
  ) where

import Ivory.Opts.AssertFold

import qualified Ivory.Language.Syntax.AST as I
import qualified Ivory.Language.Syntax.Type as I

--------------------------------------------------------------------------------

fpFold :: I.Proc -> I.Proc
fpFold = procFold "fp" (expFoldDefault fpAssert)

--------------------------------------------------------------------------------

-- We're assuming we don't have to check lits---that you'd never actually
-- construct a literal inf or NaN value!
fpAssert :: I.Type -> I.Expr -> FolderStmt ()
fpAssert ty e = case ty of
  I.TyFloat   -> asst
  I.TyDouble  -> asst
  _           -> return ()
  where asst = insert (mkAssert ty e)

mkAssert :: I.Type -> I.Expr -> I.Stmt
mkAssert ty e = I.CompilerAssert $ I.ExpOp I.ExpAnd
  [ I.ExpOp I.ExpNot [I.ExpOp (I.ExpIsNan ty) [e]]
  , I.ExpOp I.ExpNot [I.ExpOp (I.ExpIsInf ty) [e]] ]
