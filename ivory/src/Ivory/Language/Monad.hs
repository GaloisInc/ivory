{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}


module Ivory.Language.Monad (
    -- * Ivory Monad
    Ivory()
  , retProxy

    -- ** Running Functions
  , runIvory, primRunIvory
  , collect

    -- ** Effects
  , noBreak
  , noReturn
  , noAlloc

    -- ** Code Blocks
  , CodeBlock(..)
  , emits
  , emit

    -- ** Name Generation
  , freshVar
  , result
  ) where

import qualified Ivory.Language.Effects as E
import Ivory.Language.Proxy
import Ivory.Language.Type
import qualified Ivory.Language.Syntax as AST

import Control.Applicative (Applicative(..))
import Data.Monoid (Monoid(..))
import MonadLib (StateT,WriterT,Id)
import qualified MonadLib


-- Monad -----------------------------------------------------------------------

newtype Ivory (eff :: E.Effects) a = Ivory
  { unIvory :: WriterT CodeBlock (StateT Int Id) a
  } deriving (Functor,Applicative,Monad)

data CodeBlock = CodeBlock
  { blockStmts    :: AST.Block
  , blockRequires :: [AST.Require]
  , blockEnsures  :: [AST.Ensure]
  } deriving (Show)

instance Monoid CodeBlock where
  mempty = CodeBlock
    { blockStmts    = []
    , blockRequires = []
    , blockEnsures  = []
    }
  mappend l r = CodeBlock
    { blockStmts    = blockStmts l    `mappend` blockStmts r
    , blockRequires = blockRequires l `mappend` blockRequires r
    , blockEnsures  = blockEnsures l  `mappend` blockEnsures r
    }


-- | Run an Ivory block computation that could require any effect.
--
-- XXX do not export
runIvory :: Ivory (E.ProcEffects s r) a -> (a,CodeBlock)
runIvory b = primRunIvory b

primRunIvory :: Ivory (E.ProcEffects s r) a -> (a,CodeBlock)
primRunIvory m = fst (MonadLib.runM (unIvory m) 0)

-- | Collect the 'CodeBlock' for an Ivory computation.
--
-- XXX do not export
collect :: Ivory eff' a -> Ivory eff (a,CodeBlock)
collect (Ivory m) = Ivory (MonadLib.collect m)

-- | Get a 'Proxy' to the return type of an Ivory block.
--
-- XXX do not export
retProxy :: Ivory eff a -> Proxy r
retProxy _ = Proxy

-- | Add some statements to the collected block.
--
-- XXX do not export
emits :: CodeBlock -> Ivory eff ()
emits  = Ivory . MonadLib.put

-- | Emit a single statement.
--
-- XXX do not export
emit :: AST.Stmt -> Ivory eff ()
emit s = emits mempty { blockStmts = [s] }

-- | Generate a fresh variable name.
--
-- XXX do not export
freshVar :: String -> Ivory eff AST.Var
freshVar pfx = Ivory $ do
  s <- MonadLib.get
  MonadLib.set $! s + 1
  return (AST.VarName (pfx ++ show s))

-- | Name the result of an expression.
--
-- XXX do not export
result :: forall eff a. IvoryExpr a => a -> Ivory eff AST.Var
result a = do
  res <- freshVar "r"
  let ty = ivoryType (Proxy :: Proxy a)
  emit (AST.Assign ty res (unwrapExpr a))
  return res


-- Public Functions ------------------------------------------------------------

noBreak :: Ivory (E.ClearBreak eff) a -> Ivory eff a
noBreak (Ivory body) = Ivory body

noAlloc :: (innerEff ~ E.ClearAlloc outerEff)
        => Ivory innerEff a -> Ivory outerEff a
noAlloc (Ivory body) = Ivory body

noReturn :: Ivory (E.ClearReturn eff) a -> Ivory eff a
noReturn (Ivory body) = Ivory body

