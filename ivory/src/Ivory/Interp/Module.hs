module Ivory.Interp.Module where

import Ivory.Interp.Monad (Eval,addProc,addStruct)
import Ivory.Language.Syntax (Module(..), Visible (..))


-- | Import functions defined in a module.
--
-- XXX this only currently handles ivory functions, extern/imported functions
-- won't be in scope.
loadModule :: Module -> Eval ()
loadModule m = do
  mapM_ addProc   ps
  mapM_ addStruct ss
  where ps = public (modProcs m) ++ private (modProcs m)
        ss = public (modStructs m) ++ private (modStructs m)
