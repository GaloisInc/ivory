{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ConstPtrRef where

import           Control.Applicative (pure)
import           Ivory.Language

test :: Def ('[ConstRef s ('Stored (Ptr 'Global ('Stored Uint8)))] ':-> ())
test = proc "ConstPtrRef_test" $ \refptr -> body $ do
  ptr <- deref refptr
  withRef ptr
    (\ref -> do
      val <- deref ref
      store ref $ val + 1)
    (pure ())

cmodule :: Module
cmodule = package "ConstPtrRef" $ incl test
