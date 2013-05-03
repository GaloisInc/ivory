{-# LANGUAGE DeriveDataTypeable #-}

module Ivory.Interp.Error where

import Ivory.Language.Syntax (Expr,Var)

import Control.Exception (Exception,throwIO)
import Data.Typeable (Typeable)


-- | Errors thrown during evaluation.
data RuntimeError
  = TypeError String String
  | FrameStackEmpty String
  | ContStackEmpty
  | AssertFailed Expr
  | AssumeFailed Expr
  | UnboundLocal Var
    deriving (Show,Typeable)

instance Exception RuntimeError


-- | Throw a runtime type error.
typeError :: String -> String -> IO a
typeError fn msg = throwIO (TypeError fn msg)

-- | The stack was empty when it shouldn't have been.
frameStackEmpty :: String -> IO a
frameStackEmpty  = throwIO . FrameStackEmpty

-- | The stack was empty when it shouldn't have been.
contStackEmpty :: IO a
contStackEmpty  = throwIO ContStackEmpty

-- | An assertion failed.  The expression given is the one that generated the
-- assertion failure.
assertFailed :: Expr -> IO a
assertFailed  = throwIO . AssertFailed

-- | An assumption failed.  The expression given is the one that generated the
-- assumption failure.
assumeFailed :: Expr -> IO a
assumeFailed  = throwIO . AssumeFailed

-- | A local variable was used, but never defined.
unboundLocal :: Var -> IO a
unboundLocal  = throwIO . UnboundLocal
