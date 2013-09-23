{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Ivory.Language.MemArea where

import Ivory.Language.Area
import Ivory.Language.Init
import Ivory.Language.Proxy
import Ivory.Language.Ref
import Ivory.Language.Scope
import Ivory.Language.Type
import qualified Ivory.Language.Syntax as I


-- Memory Areas ----------------------------------------------------------------

-- | Externally defined memory areas.
data MemArea (area :: Area *)
  = MemImport I.AreaImport
  | MemArea I.Area
  | DynArea I.Area I.Area

-- XXX do not export
memSym :: MemArea area -> I.Sym
memSym m = case m of
  MemImport i -> I.aiSym i
  MemArea a   -> I.areaSym a
  DynArea a _ -> I.areaSym a

-- | Define a global constant.
area :: forall area. IvoryArea area
     => I.Sym -> Maybe (Init area) -> MemArea area
area sym mb = MemArea I.Area
  { I.areaSym   = sym
  , I.areaConst = False
  , I.areaType  = ivoryArea (Proxy :: Proxy area)
  , I.areaInit  = maybe I.zeroInit getInit mb
  }

-- | Import an external symbol from a header.
importArea :: IvoryArea area => I.Sym -> String -> MemArea area
importArea name header = MemImport I.AreaImport
  { I.aiSym   = name
  , I.aiConst = False
  , I.aiFile  = header
  }


-- Constant Memory Areas -------------------------------------------------------

newtype ConstMemArea (area :: Area *) = ConstMemArea (MemArea area)

-- | Constant memory area definition.
constArea :: forall area. IvoryArea area
          => I.Sym -> Init area -> ConstMemArea area
constArea sym i = ConstMemArea $ MemArea I.Area
  { I.areaSym   = sym
  , I.areaConst = True
  , I.areaType  = ivoryArea (Proxy :: Proxy area)
  , I.areaInit  = getInit i
  }

-- | Import an external symbol from a header.
importConstArea :: IvoryArea area => I.Sym -> String -> ConstMemArea area
importConstArea name header = ConstMemArea $ MemImport I.AreaImport
  { I.aiSym   = name
  , I.aiConst = False
  , I.aiFile  = header
  }

-- Area Usage ------------------------------------------------------------------

-- | Turn a memory area into a reference.
class IvoryAddrOf (mem :: Area * -> *) ref | mem -> ref, ref -> mem  where
  addrOf :: IvoryArea area => mem area -> ref Global area

-- XXX do not export
primAddrOf :: IvoryArea area => MemArea area -> I.Expr
primAddrOf mem = I.ExpAddrOfGlobal (memSym mem)

instance IvoryAddrOf MemArea Ref where
  addrOf mem = wrapExpr (primAddrOf mem)

instance IvoryAddrOf ConstMemArea ConstRef where
  addrOf (ConstMemArea mem) = wrapExpr (primAddrOf mem)

-- Dynamic Array Memory Areas --------------------------------------------------

-- | A dynamic array area contains one user visible area for the
-- "dyn_array" structure, and a hidden area for the array's storage.
makeDynArea :: forall a. IvoryArea a => I.Sym -> [Init a] -> Bool -> MemArea (DynArray a)
makeDynArea name xs isConst = DynArea arr storage
  where
    ty  = I.TyArr (length xs) (ivoryArea (Proxy :: Proxy a))
    -- NOTE: We are reserving the "_Ivory_*" namespace for internal
    -- definitions generated from user-supplied symbols.
    sym = "_Ivory_storage_" ++ name
    arr = I.Area
      { I.areaSym   = name
      , I.areaConst = isConst
      , I.areaType  = ivoryArea (Proxy :: Proxy (DynArray a))
      , I.areaInit  = I.InitDynArray ty (I.ExpAddrOfGlobal sym)
      }
    storage = I.Area
      { I.areaSym   = sym
      , I.areaConst = isConst
      , I.areaType  = ty
      , I.areaInit  = I.InitArray (map getInit xs)
      }

-- | Define a global dynamic array.  We actually need to define
-- two memory areas---one to hold the array data, and one to
-- hold the dynamic array information.
dynArea :: forall a. IvoryArea a => I.Sym -> [Init a] -> MemArea (DynArray a)
dynArea sym xs = makeDynArea sym xs False

constDynArea :: forall a. IvoryArea a => I.Sym -> [Init a] -> ConstMemArea (DynArray a)
constDynArea sym xs = ConstMemArea (makeDynArea sym xs True)
