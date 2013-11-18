{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Ivory.ModelCheck.CVC4 where

import           Prelude hiding (exp)
import           Data.List
import qualified Data.ByteString.Char8 as B

--------------------------------------------------------------------------------

type Var = String

--------------------------------------------------------------------------------
-- Concrete syntax

class Concrete a where
  concrete :: a -> B.ByteString

instance Concrete B.ByteString where
  concrete = id

instance Concrete String where
  concrete = B.pack

data ConcreteList = forall a. Concrete a => CL a

clBS :: B.ByteString -> ConcreteList
clBS = CL

--------------------------------------------------------------------------------
-- Statements

data Statement = VarDecl Var Type
--               | VarAssign Var Type Expr
               | Assert Expr
               | Query Expr
  deriving (Show)

instance Concrete Statement where
  concrete (VarDecl v ty)       = statement [CL v, clBS ":", CL ty]
  -- concrete (VarAssign v ty exp) = statement [ CL v, clBS ":", CL ty
  --                                           , clBS "=", CL exp
  --                                           ]
  concrete (Assert exp)         = statement [clBS "ASSERT", CL exp]
  concrete (Query exp)          = statement [clBS "QUERY", CL exp]

statement :: [ConcreteList] -> B.ByteString
statement as =
  let unList (CL a) = concrete a in
  let toks = B.unwords (map unList as) in
  B.snoc toks ';'

varDecl :: Var -> Type -> Statement
varDecl = VarDecl

-- varAssign :: Var -> Type -> Expr -> Statement
-- varAssign = VarAssign

assert :: Expr -> Statement
assert = Assert

query :: Expr -> Statement
query = Query

--------------------------------------------------------------------------------
-- Expressions and literals

instance Concrete Float where
  concrete = concrete . show

instance Concrete Double where
  concrete = concrete . show

instance Concrete Integer where
  concrete = concrete . show

data Type = Bool
          | Integer
          | Real
  deriving (Show, Read, Eq)

instance Concrete Type where
  concrete Bool    = "BOOLEAN"
  concrete Real    = "REAL"
  concrete Integer = "INT"

data Expr = Var Var
          -- Boolean expressions
          | T
          | F
          | Not      Expr
          | And      Expr Expr
          | Or       Expr Expr
          | Impl     Expr Expr
          | Eq       Expr Expr
          | Le       Expr Expr
          | Leq      Expr Expr
          | Ge       Expr Expr
          | Geq      Expr Expr
          -- Numeric expressions
          | forall a . (Show a, Concrete a, Num a) => NumLit a
          | Add      Expr Expr
          | Sub      Expr Expr

deriving instance Show Expr

leaf :: Expr -> Bool
leaf exp =
  case exp of
    (Var _)    -> True
    T          -> True
    F          -> True
    (NumLit _) -> True
    _          -> False

parens :: Expr -> B.ByteString
parens exp =
  if leaf exp
    then concrete exp
    else  '(' `B.cons` (concrete exp `B.snoc` ')')

instance Concrete Expr where
  concrete (Var v)      = concrete v
  concrete T            = "TRUE"
  concrete F            = "FALSE"
  concrete (Not e)      = B.unwords ["NOT", parens e]
  concrete (And e0 e1)  = B.unwords [parens e0, "AND", parens e1]
  concrete (Or e0 e1)   = B.unwords [parens e0, "OR" , parens e1]
  concrete (Impl e0 e1) = B.unwords [parens e0, "=>" , parens e1]
  concrete (Eq e0 e1)   = B.unwords [parens e0, "=" , parens e1]
  concrete (Le e0 e1)   = B.unwords [parens e0, "<" , parens e1]
  concrete (Leq e0 e1)  = B.unwords [parens e0, "<=" , parens e1]
  concrete (Ge e0 e1)   = B.unwords [parens e0, ">" , parens e1]
  concrete (Geq e0 e1)  = B.unwords [parens e0, ">=" , parens e1]
  concrete (NumLit n)   = concrete n
  concrete (Add e0 e1)  = B.unwords [parens e0, "+", parens e1]
  concrete (Sub e0 e1)  = B.unwords [parens e0, "-", parens e1]

var :: String -> Expr
var = Var

true :: Expr
true = T

false :: Expr
false = F

not' :: Expr -> Expr
not' = Not

(.&&) :: Expr -> Expr -> Expr
(.&&) = And

(.||) :: Expr -> Expr -> Expr
(.||) = Or

(.=>) :: Expr -> Expr -> Expr
(.=>) = Impl

and' :: [Expr] -> Expr
and' [] = true
and' ls = foldl1' (.&&) ls

or' :: [Expr] -> Expr
or' [] = false
or' ls = foldl1' (.||) ls

(.==) :: Expr -> Expr -> Expr
(.==) = Eq

(.<) :: Expr -> Expr -> Expr
(.<) = Le

(.<=) :: Expr -> Expr -> Expr
(.<=) = Leq

(.>) :: Expr -> Expr -> Expr
(.>) = Ge

(.>=) :: Expr -> Expr -> Expr
(.>=) = Geq

(.+) :: Expr -> Expr -> Expr
(.+) = Add

(.-) :: Expr -> Expr -> Expr
(.-) = Sub

lit :: (Show a, Concrete a, Num a) => a -> Expr
lit = NumLit

intLit :: Integer -> Expr
intLit = lit

realLit :: Integer -> Expr
realLit = lit

--------------------------------------------------------------------------------
-- Testing

foo :: Statement
foo = assert $ (intLit 3 .== var "x") .&& (var "x" .< intLit 4)
