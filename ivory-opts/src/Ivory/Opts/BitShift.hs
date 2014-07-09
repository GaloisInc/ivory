{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

--------------------------------------------------------------------------------
-- (c) Galois, Inc. 2014.
-- All rights reserved.

-- | Check for undefined bitshift behavior. Bit-shifts on signed ints are
-- already disallowed. This check is that we bit-shift by strictly less than n
-- for an n-bit value.
--------------------------------------------------------------------------------

module Ivory.Opts.BitShift
  ( bitShiftFold
  ) where

import Ivory.Opts.AssertFold

import qualified Ivory.Language.Syntax.AST   as I
import qualified Ivory.Language.Syntax.Type  as I
import Ivory.Language

--------------------------------------------------------------------------------

bitShiftFold :: I.Proc -> I.Proc
bitShiftFold = procFold "bits" (expFoldDefault bitShiftAssert)

--------------------------------------------------------------------------------

bitShiftAssert :: I.Type -> I.Expr -> FolderStmt ()
bitShiftAssert ty e = case e of
  I.ExpOp op es -> go op es
  _             -> return ()
  where
  go op es = case op of
    I.ExpBitShiftL -> assrt
    I.ExpBitShiftR -> assrt
    _              -> return ()

    where
    assrt = case ty of
      I.TyWord w -> case w of
                      I.Word8  -> mkAsst (iBitSize (0 :: Uint8))
                      I.Word16 -> mkAsst (iBitSize (0 :: Uint16))
                      I.Word32 -> mkAsst (iBitSize (0 :: Uint32))
                      I.Word64 -> mkAsst (iBitSize (0 :: Uint64))
      _          -> return ()

    mkAsst sz = insert $ I.CompilerAssert $ I.ExpOp (I.ExpLt False ty)
                  [es !! 1, fromIntegral sz]

--------------------------------------------------------------------------------
