
module Ivory.Stdlib.Trig
  ( iAtan2
  ) where

import Ivory.Language

-- spec: http://en.wikipedia.org/wiki/Atan2
iAtan2 :: (IvoryOrd a, Floating a) => a -> a -> a
iAtan2 y x = ((x >? 0)?(                   atan(y/x)
              ,((y >=? 0).&&(x <? 0))?(    atan(y/x) + pi
               ,((y <? 0).&&(x <? 0))?(    atan(y/x) - pi
                ,((y >? 0).&&(x ==? 0))?(  pi/2
                 ,((y <? 0).&&(x ==? 0))?( -1*(pi/2)
                  ,0))))))
