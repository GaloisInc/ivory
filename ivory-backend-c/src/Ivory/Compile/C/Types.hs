{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}

module Ivory.Compile.C.Types where

import Language.C.Quote.GCC
import qualified "language-c-quote" Language.C.Syntax as C

#if __GLASGOW_HASKELL__ <= 708
import Data.Monoid
import Control.Applicative
#endif

import MonadLib (WriterT,Id,put)
import qualified Data.Set as S

--------------------------------------------------------------------------------

data Include
  = SysInclude   FilePath -- ^ @#include <foo.h>@
  | LocalInclude FilePath -- ^ @#include "foo.h"@
    deriving (Show,Eq,Ord)

includeDef :: Include -> C.Definition
includeDef incl = case incl of
  SysInclude file   -> [cedecl| $esc:("#include <"  ++ file ++ ">")           |]
  LocalInclude file -> [cedecl| $esc:("#include \"" ++ file ++ "\"")          |]

type Includes = S.Set Include
type Sources  = [C.Definition]

data CompileUnits = CompileUnits
  { unitName :: String
  , sources  :: (Includes, Sources)
  , headers  :: (Includes, Sources)
  } deriving Show

instance Monoid CompileUnits where
  mempty = CompileUnits mempty mempty mempty
  (CompileUnits n0 s0 h0) `mappend` (CompileUnits n1 s1 h1) =
    CompileUnits (n0 `mappend` n1)
                 (s0 `mappend` s1)
                 (h0 `mappend` h1)

--------------------------------------------------------------------------------

newtype CompileM a = Compile
  { unCompile :: WriterT CompileUnits Id a }
  deriving (Functor, Monad, Applicative)

type Compile = CompileM ()

--------------------------------------------------------------------------------

putSrc :: C.Definition -> Compile
putSrc def = Compile (put mempty { sources = (S.empty,[def]) })

putSrcInc :: Include -> Compile
putSrcInc inc = Compile (put mempty { sources = (S.fromList [inc],[]) })

putHdrSrc :: C.Definition -> Compile
putHdrSrc hdr = Compile (put mempty { headers = (S.empty,[hdr]) })

putHdrInc :: Include -> Compile
putHdrInc inc = Compile (put mempty { headers = (S.fromList [inc],[]) })

--------------------------------------------------------------------------------
