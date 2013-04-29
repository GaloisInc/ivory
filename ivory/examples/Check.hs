{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

import Ivory.Interp
import Ivory.Interp.Tests
import Ivory.Language


main :: IO ()
main  = do
  putStrLn "Checking interpreter"
  withEnv $ do
    testInterp

    let loopBody :: (eff `Returns` Uint8) => Uint8 -> Ivory eff ()
        loopBody  = ret . (+ 1)
    io . print =<< scanlEval 10 0 loopBody
