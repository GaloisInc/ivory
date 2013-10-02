{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.Language.DynArray where

import GHC.TypeLits

import Ivory.Language.Area
import Ivory.Language.Array
import Ivory.Language.Effects
import Ivory.Language.IBool
import Ivory.Language.Init
import Ivory.Language.Monad
import Ivory.Language.Proxy
import Ivory.Language.Scope
import Ivory.Language.Ref
import Ivory.Language.Type

import qualified Ivory.Language.Syntax as I

----------------------------------------------------------------------
-- Dynamic Array Primitives

-- | Allocate a dynamic array wrapping an existing array.
toDynArray :: forall a s1 s2 eff len ref.
              ( SingI len, IvoryArea a, IvoryRef ref
              , GetAlloc eff ~ Scope s2
              , IvoryExpr (ref s1 (Array len a))
              , IvoryExpr (ref (Stack s2) (DynArray a)))
           => ref s1 (Array len a)
           -> Ivory eff (ref (Stack s2) (DynArray a))
toDynArray ref = do
  let ty = ivoryType (Proxy :: Proxy (ref (Stack s2) (DynArray a)))
  ref' <- local (idynarrayWrap ref)
  return (wrapExpr (I.ExpSafeCast ty (unwrapExpr ref')))

-- | Use a dynamic array as a sized array, calling an error
-- handler function if the array is too small.
withDynArray :: forall a b c s eff len ref.
                ( SingI len, IvoryArea a, IvoryRef ref
                , IvoryExpr (ref s (DynArray a))
                , IvoryExpr (ref s (Array len a)))
             => ref s (DynArray a)
             -> (ref s (Array len a) -> IxRep -> Ivory eff b)
             -> (IxRep -> Ivory eff c)
             -> Ivory eff ()
withDynArray arr ok_f fail_f = do
  let req_len = fromInteger (fromTypeNat (sing :: Sing len))
  let ty = ivoryType (Proxy :: Proxy (ref s (Array len a)))
  len <- deref (dynArrayLength arr)
  ifte_ (req_len <=? len)
    (do a <- assign (wrapExpr (I.ExpDynArrayData ty (unwrapExpr arr)))
        ok_f a len)
    (fail_f len)

-- Note: We are acting as though the CArray was allocated in a
-- new scope so that it cannot escape the body of the function.
withDynArrayData :: forall a b s eff ref.
                    ( IvoryArea a, IvoryRef ref
                    , IvoryExpr (ref s (DynArray a))
                    , IvoryExpr (ref s (CArray a))
                    , IvoryExpr (ref (Stack s) (CArray a)))
                 => ref s (DynArray a)
                 -> (ref (Stack s) (CArray a) -> IxRep -> Ivory eff b)
                 -> Ivory eff b
withDynArrayData arr f = do
  let ty  = ivoryType (Proxy :: Proxy (ref s (CArray a)))
  len    <- deref (dynArrayLength arr)
  carr   <- assign (wrapExpr (I.ExpDynArrayData ty (unwrapExpr arr)))
  f carr len

-- | Retrieve a reference to a dynamic array's length.
dynArrayLength :: ( IvoryArea a, IvoryRef ref
                  , IvoryExpr (ref s (DynArray a)))
               => ref s (DynArray a)
               -> ConstRef s (Stored IxRep)
dynArrayLength d = wrapExpr (I.ExpDynArrayLength (unwrapExpr d))

-- | Index a dynamic array with a run-time bounds check.  If @ix@
-- is within bounds of the array, call @ok_f@ with a reference to
-- the element at that position.  Otherwise, call @fail_f@.
dynArrayRef :: forall a b c s eff ref.
               ( IvoryArea a, IvoryRef ref
               , IvoryExpr (ref s (DynArray a))
               , IvoryExpr (ref s a))
            => ref s (DynArray a)
            -> IxRep
            -> (ref s a -> Ivory eff b)
            -> Ivory eff c
            -> Ivory eff ()
dynArrayRef arr ix ok_f fail_f = do
  elt             <- freshVar "elt"
  let eltVar       = wrapExpr (I.ExpVar elt)
  (_, ok_block)   <- collect (ok_f eltVar)
  (_, fail_block) <- collect fail_f
  let ty           = ivoryType (Proxy :: Proxy (ref s (DynArray a)))
  emit (I.DynArrayRef elt ty (unwrapExpr arr) (unwrapExpr ix)
                      (blockStmts ok_block) (blockStmts fail_block))

-- | Map a function over each element of a dynamic array.
dynArrayMap :: forall a b s eff ref.
               ( IvoryArea a, IvoryRef ref
               , IvoryExpr (ref s (DynArray a))
               , IvoryExpr (ref s a))
            => ref s (DynArray a)
            -> (ref s a -> IxRep -> Ivory eff b)
            -> Ivory eff ()
dynArrayMap arr body = do
  elt        <- freshVar "elt"
  ix         <- freshVar "ix"
  let eltVar  = wrapExpr (I.ExpVar elt)
  let ixVar   = wrapExpr (I.ExpVar ix)
  (_, block) <- collect (body eltVar ixVar)
  let ty      = ivoryType (Proxy :: Proxy (ref s (DynArray a)))
  emit (I.DynArrayMap elt ix ty (unwrapExpr arr) (blockStmts block))

