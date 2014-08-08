{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.ModelCheck.CVC4 where

import           Prelude hiding (exp)
import           Data.List (intersperse)
import           Control.Applicative
import           Data.Monoid
import           Data.Word
import           Data.Int
import qualified Data.ByteString.Char8 as B
import           MonadLib.Monads
import           GHC.Exts (IsString(..),IsList(..))

--------------------------------------------------------------------------------

type Var  = String
type Func = String

--------------------------------------------------------------------------------
-- Concrete syntax

newtype PP' a = PP
  { unPP :: Reader Int a
  } deriving (Functor,Applicative,Monad)

prec :: PP' Int
prec = PP ask

withPrec :: Int -> PP' a -> PP' a
withPrec d (PP m) = PP $ local d m

runPP :: Int -> PP' a -> a
runPP i = runReader i . unPP

instance IsString a => IsString (PP' a) where
  fromString = PP . return . fromString

type PP  = PP' B.ByteString

class Concrete a where
  concrete :: a -> B.ByteString
  concrete = runPP 0 . concs
  concs :: a -> PP
  concs = return . concrete

instance Concrete B.ByteString where
  concrete = id

instance Concrete String where
  concrete = B.pack

data Conc = forall a. Concrete a => Conc a

{-
-- Specialization
clBS :: B.ByteString -> ConcreteList
clBS = Conc
-}

--------------------------------------------------------------------------------
-- Statements

data Statement = TypeDecl Type
               | VarDecl  Var  Type
               | Assert   Expr
               | Query    Expr
               -- Arbitrary stmt constructed by-hand.
               | forall a. Concrete a => Statement a

instance IsList Statement where
  type Item Statement = B.ByteString
  fromList ss = Statement $ endSemi $ B.unwords ss

instance IsString Statement where
  fromString = Statement . B.pack

instance Concrete Statement where
  concs st = case st of
    TypeDecl  ty -> stmt $ concs ty <+> ":" <+> "TYPE"
    VarDecl v ty -> stmt $ concs v  <+> ":" <+> concs ty
    Assert   exp -> stmt $         "ASSERT" <+> concs exp
    Query    exp -> stmt $         "QUERY"  <+> concs exp
    Statement  a -> stmt $ concs a

-- | properly format a space-separated line, ended by a semicolon.
stmt :: PP -> PP
stmt = liftM endSemi

endSemi :: B.ByteString -> B.ByteString
endSemi = flip B.snoc ';'

typeDecl :: Type -> Statement
typeDecl = TypeDecl

varDecl :: Var -> Type -> Statement
varDecl = VarDecl

assert :: Expr -> Statement
assert = Assert

query :: Expr -> Statement
query = Query

(<^>) :: PP -> PP -> PP
(<^>) = liftM2 (<>)
infixr 2 <^>

(<+>) :: PP -> PP -> PP
m1 <+> m2 = do
  s1 <- m1
  s2 <- m2
  return $ s1 <> " " <> s2
infixr 2 <+>

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
          | Arr Int Type
          | Tuple [Type]
          | Fun [Type] Type
  deriving (Show, Read, Eq)

instance Concrete Type where
  concs typ = case typ of
    Bool          -> "BOOLEAN"
    Real          -> "REAL"
    Integer       -> "INT"
    (Struct name) -> str name
    Void          -> "[]"
    Char          -> "CHAR"
    Opaque        -> "OPAQUE"
    Arr n t       -> parens 10 $ "ARRAY INT OF" <+> concs t
    Fun ts r       -> parens 4
      $ parenGrp (map concs ts) <+> "->" <+> concs r
    Tuple ts      -> brackGrp $ map concs ts

data Expr
  = Var Var
  -- Boolean expressions
  | T
  | F
  | Not      Expr
  | And      Expr Expr
  | Or       Expr Expr
  | Impl     Expr Expr
  | Eq       Expr Expr
  | Lt       Expr Expr
  | Leq      Expr Expr
  | Gt       Expr Expr
  | Geq      Expr Expr
  -- Numeric expressions
  | Add      Expr Expr
  | Sub      Expr Expr
  | Mul      Expr Expr
  | Cond     Expr Expr Expr
  | Abs      Expr
  | Signum   Expr
  | Div      Expr Expr
  | forall a . (Show a, Concrete a, Num a) => NumLit a
  -- Void, Function, Array
  | Unit
  | Call     Func [Expr]
  | Index    Expr Expr

deriving instance Show Expr

parens :: Int -> PP -> PP
parens d m = do
  p <- prec
  let f = if p > d then wrap "(" ")" else id
  f $ withPrec (succ d) m

sep :: B.ByteString -> [PP] -> PP
sep s = liftM B.concat . sequence . intersperse (return s)

commaSep :: [PP] -> PP
commaSep = sep ", "

wrap :: B.ByteString -> B.ByteString -> PP -> PP
wrap o c m = do
  s <- m
  return $ o <> s <> c

parWrap :: PP -> PP
parWrap = wrap "(" ")"

brkWrap :: PP -> PP
brkWrap = wrap "[" "]"

parenGrp :: [PP] -> PP
parenGrp = parWrap . commaSep

brackGrp :: [PP] -> PP
brackGrp = brkWrap . commaSep

spaces :: [PP] -> PP
spaces = juxt . intersperse (return " ")

juxt :: [PP] -> PP
juxt = liftM B.concat . sequence

str :: String -> PP
str = return . B.pack

instance Concrete Expr where
  concs exp = case exp of
    Var v       -> concs v
    T           -> "TRUE"
    F           -> "FALSE"
    NumLit   n  -> concs n
    Not e       -> parens 10 $              "NOT" <+> concs e
    And   e0 e1 -> parens 10 $ concs e0 <+> "AND" <+> concs e1
    Or    e0 e1 -> parens 10 $ concs e0 <+> "OR"  <+> concs e1
    Impl  e0 e1 -> parens 10 $ concs e0 <+> "=>"  <+> concs e1
    Eq    e0 e1 -> parens 10 $ concs e0 <+> "="   <+> concs e1
    Lt    e0 e1 -> parens 10 $ concs e0 <+> "<"   <+> concs e1
    Leq   e0 e1 -> parens 10 $ concs e0 <+> "<="  <+> concs e1
    Gt    e0 e1 -> parens 10 $ concs e0 <+> ">"   <+> concs e1
    Geq   e0 e1 -> parens 10 $ concs e0 <+> ">="  <+> concs e1
    Add   e0 e1 -> parens 10 $ concs e0 <+> "+"   <+> concs e1
    Sub   e0 e1 -> parens 10 $ concs e0 <+> "-"   <+> concs e1
    Mul   e0 e1 -> parens 10 $ concs e0 <+> "*"   <+> concs e1
    Div   e0 e1 -> parens 10 $ concs e0 <+> "/"   <+> concs e1
    Index e0 e1 -> parens 10 $ concs e0 <^> "[" <^> concs e1 <^> "]"
    Call f args -> parens 10 $ str f <^> brackGrp (map concs args)

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

(.==) :: Expr -> Expr -> Expr
(.==) = Eq

(.<) :: Expr -> Expr -> Expr
(.<) = Lt

(.<=) :: Expr -> Expr -> Expr
(.<=) = Leq

(.>) :: Expr -> Expr -> Expr
(.>) = Gt

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

charLit :: Char -> Expr
charLit = intLit . toEnum . fromEnum

realLit :: Integer -> Expr
realLit = lit

nullLit :: Expr
nullLit = Unit

call :: Func -> [Expr] -> Expr
call = Call

--------------------------------------------------------------------------------
-- CVC4 Lib

----------------------------------------
-- Bounded int types

boundedFunc :: forall a . (Show a, Integral a, Bounded a)
                => Func -> a -> Statement
boundedFunc f _sz = 
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
-- Types

charType :: Statement
charType = "CHAR : TYPE = INT"

opaqueType :: Statement
opaqueType = "OPAQUE : TYPE"

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
modFunc = 
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
  , int8Bound ,  int16Bound,  int32Bound,  int64Bound
  , charType  , opaqueType , modFunc
  ]

--------------------------------------------------------------------------------
-- Testing

foo :: Statement
foo = assert $ (intLit 3 .== var "x") .&& (var "x" .< intLit 4)

