{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

module Ivory.Compile.C.Types where

import Language.C.Quote.GCC
import qualified "language-c-quote" Language.C.Syntax as C

import MonadLib (WriterT,Id,put)
import Data.Monoid
import System.FilePath.Posix ((</>)) -- Always use Posix conventions in C
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
  , externs  :: Sources
  } deriving Show

instance Monoid CompileUnits where
  mempty = CompileUnits mempty mempty mempty mempty
  (CompileUnits n0 s0 h0 e0) `mappend` (CompileUnits n1 s1 h1 e1) =
    CompileUnits (n0 `mappend` n1)
                 (s0 `mappend` s1)
                 (h0 `mappend` h1)
                 (e0 `mappend` e1)

--------------------------------------------------------------------------------

newtype CompileM a = Compile
  { unCompile :: WriterT CompileUnits Id a }
  deriving (Functor, Monad)

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

putExt :: C.Definition -> Compile
putExt ext = Compile (put mempty { externs = [ext] })

--------------------------------------------------------------------------------
