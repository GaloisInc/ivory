{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Ivory.Language.MemArea where

import Ivory.Language.Area
import Ivory.Language.Init
import Ivory.Language.Monad
import Ivory.Language.Proxy
import Ivory.Language.Ref
import Ivory.Language.Scope
import Ivory.Language.Type
import qualified Ivory.Language.Syntax as I


-- Memory Areas ----------------------------------------------------------------

-- | Externally defined memory areas.
data MemArea (area :: Area)
  = MemImport I.AreaImport
  | MemArea I.Area

-- XXX do not export
memSym :: MemArea area -> I.Sym
memSym m = case m of
  MemImport i -> I.aiSym i
  MemArea a   -> I.areaSym a

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

newtype ConstMemArea (area :: Area) = ConstMemArea (MemArea area)

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

class IvoryAddrOf (mem :: Area -> *) ref | mem -> ref, ref -> mem  where
  addrOf :: IvoryArea area => mem area -> Ivory eff (ref Global area)

-- XXX do not export
primAddrOf :: forall area eff. IvoryArea area => MemArea area -> Ivory eff I.Var
primAddrOf mem = do
  rname <- freshVar "ref"
  let areaTy = ivoryArea (Proxy :: Proxy area)
  emit (I.AllocRef areaTy rname (I.NameSym (memSym mem)))
  return rname

instance IvoryAddrOf MemArea Ref where
  addrOf mem = wrapVar `fmap` primAddrOf mem

instance IvoryAddrOf ConstMemArea ConstRef where
  addrOf (ConstMemArea mem) = wrapVar `fmap` primAddrOf mem
