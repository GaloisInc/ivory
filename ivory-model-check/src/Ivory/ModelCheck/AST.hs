{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.ModelCheck.AST where

import Prelude ()
import Prelude.Compat hiding (exp)
import Data.Monoid.Compat

import qualified Data.Bits as Bits
import qualified Ivory.Language.Syntax.Type as I
import           Ivory.Language.Syntax.Concrete.Location
import           Ivory.Language.Syntax.Concrete.Pretty
import           MonadLib (Id,runId)

--------------------------------------------------------------------------------

type Var  = String
type Func = String

--------------------------------------------------------------------------------
-- Statements

data Statement = TypeDecl String [(Var, Type)]
               | VarDecl Var Type
               | Assert (Located Expr)
               | Query (Located Expr)
                 deriving (Show)


--------------------------------------------------------------------------------
-- Expressions and literals

data Type = Void
          | SBV !I.IntSize
          | UBV !I.WordSize
          | Real
          | Char
          | Bool
          | Struct String
          | Array Type
          | Opaque
            deriving (Show,Eq)

data Expr = Var Var

          -- Boolean expressions
          | T
          | F
          | Not      Expr
          | And      Expr Expr
          | Or       Expr Expr
          | Impl     Expr Expr

          | Eq       Expr Expr
          | Le  Type Expr Expr
          | Leq Type Expr Expr
          | Ge  Type Expr Expr
          | Geq Type Expr Expr

          -- Numeric expressions
          | IntLit   Integer
          | RealLit  Rational
          | Add Type Expr Expr
          | Sub Type Expr Expr
          | Mod Type Expr Expr
          | Call     Func [Expr]
          | Store    Expr Expr
          | Field    Expr Expr
          | Index    Expr Expr
            deriving (Show,Eq)

-- | Child traversal for expressions.
traverseExpr :: Applicative f => (Expr -> f Expr) -> (Expr -> f Expr)
traverseExpr _ e@Var{}     = pure e
traverseExpr _ T           = pure T
traverseExpr _ F           = pure F
traverseExpr _ e@IntLit{}  = pure e
traverseExpr _ e@RealLit{} = pure e
traverseExpr f (Not e)     = Not   <$> f e
traverseExpr f (And a b)   = And   <$> f a <*> f b
traverseExpr f (Or a b)    = Or    <$> f a <*> f b
traverseExpr f (Impl a b)  = Impl  <$> f a <*> f b
traverseExpr f (Eq a b)    = Eq    <$> f a <*> f b
traverseExpr f (Le t a b)  = Le t  <$> f a <*> f b
traverseExpr f (Leq t a b) = Leq t <$> f a <*> f b
traverseExpr f (Ge t a b)  = Ge t  <$> f a <*> f b
traverseExpr f (Geq t a b) = Geq t <$> f a <*> f b
traverseExpr f (Add t a b) = Add t <$> f a <*> f b
traverseExpr f (Sub t a b) = Sub t <$> f a <*> f b
traverseExpr f (Mod t a b) = Mod t <$> f a <*> f b
traverseExpr f (Call i xs) = Call i <$> traverse f xs
traverseExpr f (Store a b) = Store <$> f a <*> f b
traverseExpr f (Field a b) = Field <$> f a <*> f b
traverseExpr f (Index a b) = Index <$> f a <*> f b


-- | Variable substitution.
substExpr :: [(Var, Expr)] -> Expr -> Expr
substExpr su = rewriteOf traverseExpr subst
  where
  subst (Var v) = lookup v su
  subst e       = Nothing


-- | Whole-expression simplification.
simplify :: Expr -> Expr
simplify  = rewriteOf traverseExpr simplify1

-- | One-step simplification.
simplify1 :: Expr -> Maybe Expr

-- re-associate Add
simplify1 (Add t (Add _ a b) c) = Just (Add t a (Add t b c))

-- boolean simplifications
simplify1 (Not (Not e)) = Just e
simplify1 (Impl T e)    = Just e
simplify1 (And F _)     = F
simplify1 (And _ F)     = F
simplify1 (Or  T _)     = T
simplify1 (Or  _ T)     = T

-- numeric simplification
simplify1 (Add t (IntLit  a) (IntLit  b)) = Just (intLit t (a + b))
simplify1 (Add t (RealLit a) (RealLit b)) = Just (RealLit  (a + b))
simplify1 (Sub t (IntLit  a) (IntLit  b)) = Just (intLit t (a - b))
simplify1 (Sub t (RealLit a) (RealLit b)) = Just (RealLit  (a - b))

simplify1 (Mod _ e (IntLit 1)) = Just (IntLit 0)

-- move constants to the right
simplify1 (Add t a@IntLit{}  b) = Just (Add t b a)
simplify1 (Add t a@RealLit{} b) = Just (Add t b a)
simplify1 (Sub t a@IntLit{}  b) = Just (Sub t b a)
simplify1 (Sub t a@RealLit{} b) = Just (Sub t b a)

simplify1 e = Nothing


signedLit :: Int -> Integer -> Integer
signedLit bits n =
  let n' = n Bits..&. (Bits.bit bits - 1)
   in if Bits.testBit n' (bits - 1)
         then (negate (Bits.bit (bits - 1))) + (Bits.clearBit n' (bits - 1))
         else n'

intLit :: Type -> Integer -> Expr
intLit (SBV I.Int8)   n = IntLit (signedLit 8  n)
intLit (SBV I.Int16)  n = IntLit (signedLit 16 n)
intLit (SBV I.Int32)  n = IntLit (signedLit 32 n)
intLit (SBV I.Int64)  n = IntLit (signedLit 64 n)
intLit (UBV I.Word8)  n = IntLit (n `mod` 255)
intLit (UBV I.Word16) n = IntLit (n `mod` 65535)
intLit (UBV I.Word32) n = IntLit (n `mod` 4294967295)
intLit (UBV I.Word64) n = IntLit (n `mod` 18446744073709551615)


-- Lens Utilities --------------------------------------------------------------

type ASetter s t a b = (a -> Id b) -> (s -> Id t)

type ASetter' s a = ASetter s s a a

over :: ASetter s t a b -> (a -> b) -> (s -> t)
over l f = \s -> runId (l (\a -> pure (f a)) s)
{-# INLINE over #-}

rewriteOf :: ASetter' a a -> (a -> Maybe a) -> (a -> a)
rewriteOf l f = go
  where
  go = transformOf l (\x -> maybe x go (f x))
{-# INLINE rewriteOf #-}

transformOf :: ASetter' a a -> (a -> a) -> (a -> a)
transformOf l f = go
  where
  go = f . over l go
{-# INLINE transformOf #-}
