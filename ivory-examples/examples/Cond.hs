{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Cond where

import Ivory.Language


add :: Def ('[Uint32,Uint32] :-> Uint32)
add  = proc "add"
     $ \ x y -> ensures (\r -> r ==? x + y)
              $ body
              $ ret (x + y)

cmodule :: Module
cmodule = package "cond" $ incl add
