{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module ConstRef where

import Ivory.Language

test :: Def ('[ConstRef s ('Stored Uint8)] ':-> Uint8)
test = proc "test" $ \r -> body $ ret =<< deref r

cmodule :: Module
cmodule = package "ConstRef" $ incl test
