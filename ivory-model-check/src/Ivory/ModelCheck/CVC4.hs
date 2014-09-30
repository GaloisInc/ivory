{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.ModelCheck.CVC4 where

import           Prelude hiding (exp)
import           Data.List (intersperse)
import           Data.Word
import           Data.Int
import qualified Data.ByteString.Char8 as B

--------------------------------------------------------------------------------

type Var  = String
type Func = String

--------------------------------------------------------------------------------
-- Concrete syntax

class Concrete a where
  concrete :: a -> B.ByteString

instance Concrete B.ByteString where
  concrete = id

instance Concrete String where
  concrete = B.pack

data ConcreteList = forall a. Concrete a => CL a

-- Specialization
clBS :: B.ByteString -> ConcreteList
clBS = CL

--------------------------------------------------------------------------------
-- Statements

data Statement = TypeDecl Type
               | VarDecl Var Type
               | Assert Expr
               | Query Expr
               -- Arbitrary statement constructed by-hand.
               | forall a . Concrete a => Statement a

instance Concrete Statement where
  concrete (TypeDecl ty)   = statement [CL ty, clBS ":", clBS "TYPE"]
  concrete (VarDecl v ty)  = statement [CL v, clBS ":", CL ty]
  concrete (Assert exp)    = statement [clBS "ASSERT", CL exp]
  concrete (Query exp)     = statement [clBS "QUERY", CL exp]
  concrete (Statement a)   = statement [CL a]

statement :: [ConcreteList] -> B.ByteString
statement as =
  let unList (CL a) = concrete a in
  let toks = B.unwords (map unList as) in
  B.snoc toks ';'

typeDecl :: Type -> Statement
typeDecl = TypeDecl

varDecl :: Var -> Type -> Statement
varDecl = VarDecl

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

data Type = Void
          | Integer
          | Real
          | Char
          | Bool
          | Struct String
          | Opaque
  deriving (Show, Read, Eq)

instance Concrete Type where
  concrete Bool          = "BOOLEAN"
  concrete Real          = "REAL"
  concrete Integer       = "INT"
  concrete (Struct name) = B.pack name

data Expr = Var Var
          -- Boolean expressions
          | T
          | F
          | Not      Expr
          | And      Expr Expr
          | Or       Expr Expr
          | Impl     Expr Expr
          | Equiv    Expr Expr
          | Eq       Expr Expr
          | Le       Expr Expr
          | Leq      Expr Expr
          | Ge       Expr Expr
          | Geq      Expr Expr
          -- Numeric expressions
          | forall a . (Show a, Concrete a, Num a) => NumLit a
          | Add      Expr Expr
          | Sub      Expr Expr
          | Mul      Expr Expr
          | Div      Expr Expr
          | Call     Func [Expr]

deriving instance Show Expr

substExpr :: [(Var, Expr)] -> Expr -> Expr
substExpr su = go
  where
  go (Var v)     = case lookup v su of
                     Nothing -> Var v
                     Just e  -> e
  go (Not e)     = Not (go e)
  go (And x y)   = And (go x) (go y)
  go (Or x y)    = Or (go x) (go y)
  go (Impl x y)  = Impl (go x) (go y)
  go (Equiv x y) = Equiv (go x) (go y)
  go (Eq x y)    = Eq (go x) (go y)
  go (Le x y)    = Le (go x) (go y)
  go (Leq x y)   = Leq (go x) (go y)
  go (Ge x y)    = Ge (go x) (go y)
  go (Geq x y)   = Geq (go x) (go y)
  go (Add x y)   = Add (go x) (go y)
  go (Sub x y)   = Sub (go x) (go y)
  go (Call f es) = Call f (map go es)
  go e           = e

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
  concrete (Var v)       = concrete v
  concrete T             = "TRUE"
  concrete F             = "FALSE"
  concrete (Not e)       = B.unwords ["NOT", parens e]
  concrete (And e0 e1)   = B.unwords [parens e0, "AND", parens e1]
  concrete (Or e0 e1)    = B.unwords [parens e0, "OR" , parens e1]
  concrete (Impl e0 e1)  = B.unwords [parens e0, "=>" , parens e1]
  concrete (Equiv e0 e1) = B.unwords [parens e0, "<=>", parens e1]
  concrete (Eq e0 e1)    = B.unwords [parens e0, "=" , parens e1]
  concrete (Le e0 e1)    = B.unwords [parens e0, "<" , parens e1]
  concrete (Leq e0 e1)   = B.unwords [parens e0, "<=" , parens e1]
  concrete (Ge e0 e1)    = B.unwords [parens e0, ">" , parens e1]
  concrete (Geq e0 e1)   = B.unwords [parens e0, ">=" , parens e1]
  concrete (NumLit n)    = concrete n
  concrete (Add e0 e1)   = B.unwords [parens e0, "+", parens e1]
  concrete (Sub e0 e1)   = B.unwords [parens e0, "-", parens e1]
  -- FIXME: probably shouldn't rely on SMT to handle any form of non-linear arithmetic
  concrete (Mul e0 e1)   = B.unwords [parens e0, "*", parens e1]
  concrete (Div e0 e1)   = B.unwords [parens e0, "/", parens e1]
  concrete (Call f args) = concrete f
                `B.append` ('(' `B.cons` (args' `B.snoc` ')'))
    where
    args' = B.unwords $ intersperse "," (map concrete args)

var :: Var -> Expr
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

(.<=>) :: Expr -> Expr -> Expr
(.<=>) = Equiv

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

(.*) :: Expr -> Expr -> Expr
(.*) = Mul

(./) :: Expr -> Expr -> Expr
(./) = Div

lit :: (Show a, Concrete a, Num a) => a -> Expr
lit = NumLit

intLit :: Integer -> Expr
intLit = lit

realLit :: Integer -> Expr
realLit = lit

call :: Func -> [Expr] -> Expr
call = Call

--------------------------------------------------------------------------------
-- CVC4 Lib

----------------------------------------
-- Bounded int types

boundedFunc :: forall a . (Show a, Integral a, Bounded a)
                => Func -> a -> Statement
boundedFunc f _sz = Statement $ B.unwords
  [ B.pack f, ":", "INT", "->", "BOOLEAN"
  , "=", "LAMBDA", "(x:INT)", ":"
  , exp (toInt minBound) (toInt maxBound)
  ]
  where
  toInt a = fromIntegral (a :: a)
  x = var "x"
  exp l h = concrete $ (intLit l .<= x) .&& (x .<= intLit h)

word8, word16, word32, word64, int8, int16, int32, int64 :: Func
word8  = "word8"
word16 = "word16"
word32 = "word32"
word64 = "word64"
int8   = "int8"
int16  = "int16"
int32  = "int32"
int64  = "int64"

word8Bound  :: Statement
word8Bound  = boundedFunc word8  (0 :: Word8)
word16Bound :: Statement
word16Bound = boundedFunc word16 (0 :: Word16)
word32Bound :: Statement
word32Bound = boundedFunc word32 (0 :: Word32)
word64Bound :: Statement
word64Bound = boundedFunc word64 (0 :: Word64)
int8Bound   :: Statement
int8Bound   = boundedFunc int8    (0 :: Int8)
int16Bound  :: Statement
int16Bound  = boundedFunc int16   (0 :: Int16)
int32Bound  :: Statement
int32Bound  = boundedFunc int32   (0 :: Int32)
int64Bound  :: Statement
int64Bound  = boundedFunc int64   (0 :: Int64)

----------------------------------------
-- Mod

modAbs :: Func
modAbs = "mod"

-- | Abstraction: a % b (C semantics) implies
--
-- (   ((a >= 0) && (a % b >= 0) && (a % b < b) && (a % b <= a))
--  || ((a < 0)  && (a % b <= 0) && (a % b > b) && (a % b >= a)))
--
-- a % b is abstracted with a fresh var v.
modFunc :: Statement
modFunc = Statement $ B.unwords
  [ B.pack modAbs, ":", "(INT, INT, INT)", "->", "BOOLEAN"
  , "=", "LAMBDA", "(v:INT, a:INT, b:INT)", ":"
  , concrete exp
  ]
  where
  v = var "v"
  a = var "a"
  b = var "b"
  z = intLit 0
  exp =   ((a .>= z) .&& (v .>= z) .&& (v .< b) .&& (v .<= a))
      .|| ((a .< z)  .&& (v .<= z) .&& (v .> b) .&& (v .>= a))

cvc4Lib :: [Statement]
cvc4Lib =
  [ word8Bound, word16Bound, word32Bound, word64Bound
  , int8Bound,  int16Bound,  int32Bound,  int64Bound
  , modFunc
  ]

--------------------------------------------------------------------------------
-- Testing

foo :: Statement
foo = assert $ (intLit 3 .== var "x") .&& (var "x" .< intLit 4)
