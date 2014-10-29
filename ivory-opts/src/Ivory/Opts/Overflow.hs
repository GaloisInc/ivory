{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

--------------------------------------------------------------------------------
-- | Assert folding: add asserts expression overflow/underflow.
--------------------------------------------------------------------------------

module Ivory.Opts.Overflow
  ( overflowFold, addBase, subBase, mulBase, divBase, (<+>), ext
  ) where

import Ivory.Opts.AssertFold

import qualified Ivory.Language.Array        as I
import qualified Ivory.Language.Syntax.AST   as I
import qualified Ivory.Language.Syntax.Type  as I
import qualified Ivory.Language.Syntax.Names as I
import qualified Ivory.Language.Type         as T
import Ivory.Language

import Prelude hiding (max,min)
import Data.Word
import Data.Int

--------------------------------------------------------------------------------

overflowFold :: I.Proc -> I.Proc
overflowFold = procFold "ovf" (expFoldDefault arithAssert)

--------------------------------------------------------------------------------

type Bounds a = (a,a)

arithAssert :: I.Type -> I.Expr -> FolderStmt ()
arithAssert ty e = case e of
  I.ExpLit i       -> litAssert ty i -- Should be impossible to fail, if all
                                     -- initializers have been accounted for.
  I.ExpOp op args  -> arithAssert' ty op args
  _                -> return ()

litAssert :: I.Type -> I.Literal -> FolderStmt ()
litAssert ty lit = case lit of
  I.LitInteger i ->
    case ty of
      I.TyWord I.Word8  -> boundLit (minMax :: Bounds Word8)
      I.TyWord I.Word16 -> boundLit (minMax :: Bounds Word16)
      I.TyWord I.Word32 -> boundLit (minMax :: Bounds Word32)
      I.TyWord I.Word64 -> boundLit (minMax :: Bounds Word64)
      I.TyInt I.Int8    -> boundLit (minMax :: Bounds Int8)
      I.TyInt I.Int16   -> boundLit (minMax :: Bounds Int16)
      I.TyInt I.Int32   -> boundLit (minMax :: Bounds Int32)
      I.TyInt I.Int64   -> boundLit (minMax :: Bounds Int64)
      I.TyIndex n       -> boundLit (0 :: Integer, n)
      _                 -> return ()
      where
      boundLit (min,max) = insert ca
        where
        ca  = I.CompilerAssert (T.unwrapExpr res)
        res = if fromIntegral min <= i && i <= fromIntegral max
                then true
                else false

      minMax :: forall t . (Bounded t) => Bounds t
      minMax = (minBound :: t, maxBound :: t)

  _ -> return ()

arithAssert' :: I.Type -> I.ExpOp -> [I.Expr] -> FolderStmt ()
arithAssert' ty op args =
  case op of
    I.ExpAdd -> case ty of
      I.TyWord I.Word8  -> mkCall addBase ty args
      I.TyWord I.Word16 -> mkCall addBase ty args
      I.TyWord I.Word32 -> mkCall addBase ty args
      I.TyWord I.Word64 -> mkCall addBase ty args
      I.TyInt I.Int8    -> mkCall addBase ty args
      I.TyInt I.Int16   -> mkCall addBase ty args
      I.TyInt I.Int32   -> mkCall addBase ty args
      I.TyInt I.Int64   -> mkCall addBase ty args
      I.TyIndex _       -> mkCall addBase ty args
      _                 -> return ()

    I.ExpSub -> case ty of
      I.TyWord I.Word8  -> mkCall subBase ty args
      I.TyWord I.Word16 -> mkCall subBase ty args
      I.TyWord I.Word32 -> mkCall subBase ty args
      I.TyWord I.Word64 -> mkCall subBase ty args
      I.TyInt I.Int8    -> mkCall subBase ty args
      I.TyInt I.Int16   -> mkCall subBase ty args
      I.TyInt I.Int32   -> mkCall subBase ty args
      I.TyInt I.Int64   -> mkCall subBase ty args
      I.TyIndex _       -> mkCall subBase ty args
      _                 -> return ()

    I.ExpMul -> case ty of
      I.TyWord I.Word8  -> mkCall mulBase ty args
      I.TyWord I.Word16 -> mkCall mulBase ty args
      I.TyWord I.Word32 -> mkCall mulBase ty args
      I.TyWord I.Word64 -> mkCall mulBase ty args
      I.TyInt I.Int8    -> mkCall mulBase ty args
      I.TyInt I.Int16   -> mkCall mulBase ty args
      I.TyInt I.Int32   -> mkCall mulBase ty args
      I.TyInt I.Int64   -> mkCall mulBase ty args
      I.TyIndex _       -> mkCall mulBase ty args
      _                 -> return ()

    I.ExpDiv -> case ty of
      I.TyWord I.Word8  -> mkCall divBase ty args
      I.TyWord I.Word16 -> mkCall divBase ty args
      I.TyWord I.Word32 -> mkCall divBase ty args
      I.TyWord I.Word64 -> mkCall divBase ty args
      I.TyInt I.Int8    -> mkCall divBase ty args
      I.TyInt I.Int16   -> mkCall divBase ty args
      I.TyInt I.Int32   -> mkCall divBase ty args
      I.TyInt I.Int64   -> mkCall divBase ty args
      I.TyIndex _       -> mkCall divBase ty args
      _                 -> return ()

    I.ExpMod -> case ty of
      I.TyWord I.Word8  -> mkCall divBase ty args
      I.TyWord I.Word16 -> mkCall divBase ty args
      I.TyWord I.Word32 -> mkCall divBase ty args
      I.TyWord I.Word64 -> mkCall divBase ty args
      I.TyInt I.Int8    -> mkCall divBase ty args
      I.TyInt I.Int16   -> mkCall divBase ty args
      I.TyInt I.Int32   -> mkCall divBase ty args
      I.TyInt I.Int64   -> mkCall divBase ty args
      I.TyIndex _       -> mkCall divBase ty args
      _                 -> return ()

    _ -> return ()

----------------------------------------------------------

--------------------------------------------------------------------------------
-- Foreign function calls to Ivory standard lib with overflow functions.

mkCall :: String -> I.Type -> [I.Expr] -> FolderStmt ()
mkCall f ty args = do
  var <- freshVar
  let v = I.VarInternal var
  insert $ I.Call I.TyBool (Just v) (I.NameSym $ f <+> ext ty)
             (map (I.Typed ty) args)
  insert $ I.CompilerAssert (I.ExpVar v)

--------------------------------------------------------------------------------
-- Construct the names of overflow checking functions defined in ivory.h.

(<+>) :: String -> String -> String
a <+> b = a ++ "_" ++ b

mkOvf :: String -> String
mkOvf a = a <+> "ovf"

addBase, subBase, mulBase, divBase :: String
addBase = mkOvf "add"
subBase = mkOvf "sub"
mulBase = mkOvf "mul"
divBase = mkOvf "div"

ext :: I.Type -> String
ext ty = case ty of
  I.TyChar
    -> "char"
  I.TyFloat
    -> "float"
  I.TyDouble
    -> "double"
  I.TyInt i
    -> case i of
         I.Int8   -> "i8"
         I.Int16  -> "i16"
         I.Int32  -> "i32"
         I.Int64  -> "i64"
  I.TyWord w
    -> case w of
         I.Word8  ->  "u8"
         I.Word16 ->  "u16"
         I.Word32  -> "u32"
         I.Word64  -> "u64"
  I.TyIndex _ -> ext I.ixRep
  _ -> error $ "Unexpected type " ++ show ty ++ " in ext."
