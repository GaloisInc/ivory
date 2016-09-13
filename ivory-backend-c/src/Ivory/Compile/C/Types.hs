{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE QuasiQuotes                #-}

module Ivory.Compile.C.Types where

import           Prelude              ()
import           Prelude.Compat

import           Data.List            (nub)

import           Language.C.Quote.GCC
import qualified "language-c-quote" Language.C.Syntax    as C

import           MonadLib             (Id, WriterT, put, runM)

--------------------------------------------------------------------------------

data Include
  = SysInclude   FilePath -- ^ @#include <foo.h>@
  | LocalInclude FilePath -- ^ @#include "foo.h"@
    deriving (Show,Eq,Ord)

includeDef :: Include -> C.Definition
includeDef incl = case incl of
  SysInclude file   -> [cedecl| $esc:("#include <"  ++ file ++ ">")           |]
  LocalInclude file -> [cedecl| $esc:("#include \"" ++ file ++ "\"")          |]

type Includes = [Include]
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
                 (go (s0 `mappend` s1))
                 (go (h0 `mappend` h1))
    where
    go (i,s) = (nub i, nub s)

--------------------------------------------------------------------------------

newtype CompileM a = Compile
  { unCompile :: WriterT CompileUnits Id a }
  deriving (Functor, Monad, Applicative)

type Compile = CompileM ()

-- | Run the monad and nub the lists. (We have lists here rather than sets since
-- we do not want to reorder headers. Sometimes a user wants headers to be
-- included in exactly the correct order, since in some (bad!) build
-- environments, includes depend on previous includes and aren't
-- self-sufficient.
runResult :: CompileM a -> CompileUnits
runResult c =
  let cu = snd (runM (unCompile c)) in
  let go (i,s) = (nub i, s) in
  cu { sources = go (sources cu)
     , headers = go (headers cu)
     }

--------------------------------------------------------------------------------

putSrc :: C.Definition -> Compile
putSrc def = Compile (put mempty { sources = ([],[def]) })

putSrcInc :: Include -> Compile
putSrcInc inc = Compile (put mempty { sources = ([inc],[]) })

putHdrSrc :: C.Definition -> Compile
putHdrSrc hdr = Compile (put mempty { headers = ([],[hdr]) })

putHdrInc :: Include -> Compile
putHdrInc inc = Compile (put mempty { headers = ([inc],[]) })

--------------------------------------------------------------------------------
