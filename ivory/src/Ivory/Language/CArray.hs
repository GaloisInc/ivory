{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ivory.Language.CArray where

import Ivory.Language.Area
import Ivory.Language.Pointer
import Ivory.Language.Proxy
import Ivory.Language.Struct
import Ivory.Language.Type
import qualified Ivory.Language.Syntax as I

--------------------------------------------------------------------------------

instance IvoryArea a => IvoryArea ('CArray a) where
  ivoryArea _ = I.TyCArray (ivoryArea (Proxy :: Proxy a))


-- | Guard invocations of toCArray.
class (IvoryArea area, IvoryArea rep)
  => ToCArray (area :: Area *) (rep :: Area *) | area -> rep

instance (ANat len, ToCArray area rep)
    => ToCArray ('Array len area) ('CArray rep)
instance IvoryType a => ToCArray ('Stored a) ('Stored a)
instance IvoryStruct sym => ToCArray ('Struct sym) ('Struct sym)

-- | Convert from a checked array to one that can be given to a c function.
toCArray :: forall s len area rep c.
            (ANat len, KnownConstancy c, ToCArray area rep)
         => Pointer 'Valid c s ('Array len area)
         -> Pointer 'Valid c s ('CArray rep)
toCArray ref = wrapExpr $ I.ExpSafeCast ty (unwrapExpr ref)
  where ty = ivoryType (Proxy :: Proxy (Pointer 'Valid c s ('CArray rep)))
