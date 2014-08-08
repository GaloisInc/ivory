{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.ModelCheck.CVC4 where

import           Prelude hiding (exp)
import           Data.List (intersperse)
import           Data.Monoid
import           Data.Word
import           Data.Int
import qualified Data.ByteString.Char8 as B
import           MonadLib.Monads

--------------------------------------------------------------------------------

type Var  = String
type Func = String

--------------------------------------------------------------------------------
-- Concrete syntax

class Concrete a where
  concrete :: a -> B.ByteString
  concrete = runReader 0 . concPrec
  concPrec :: a -> Reader Int B.ByteString
  concPrec = return . concrete

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

instance Concrete Statement where
  concrete st = case st of
    TypeDecl  ty -> stmt [concrete ty, ":", "TYPE"]
    VarDecl v ty -> stmt [concrete v , ":", concrete ty]
    Assert   exp -> stmt ["ASSERT"   , concrete exp]
    Query    exp -> stmt ["QUERY"    , concrete exp]
    Statement  a -> stmt [concrete a]

-- | properly format a space-separated line, ended by a semicolon.
stmt :: [B.ByteString] -> B.ByteString
stmt = flip B.snoc ';' . B.unwords

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
          | Arr Int Type
          | Tuple [Type]
          | Fun [Type] Type
  deriving (Show, Read, Eq)

instance Concrete Type where
  concPrec typ = case typ of
    Bool          -> return "BOOLEAN"
    Real          -> return "REAL"
    Integer       -> return "INT"
    (Struct name) -> return $ B.pack name
    Void          -> return "[]"
    Char          -> return "CHAR"
    Opaque        -> return "OPAQUE"
    Arr n t       -> parens' 10 $ do
      s <- concPrec t
      return $ "ARRAY INT OF " <> s
    Fun ts r       -> parens' 4 $ do
      s1 <- concPrec a
      s2 <- concPrec b
      return $ s1 <> " -> " <> s2
    Tuple ts      -> return
       $ "["
      <> B.concat
       ( intersperse ", "
       $ map concrete ts
       )
      <> "]"

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
          | Call     Func [Expr]

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

parens' :: Int
       -> Reader Int B.ByteString
       -> Reader Int B.ByteString
parens' d m = do
  prec <- ask
  if prec > d
    then do
      s <- m'
      return $ "(" <> s <> ")"
    else m'
  where
  m' = local (succ d) m

sep :: B.ByteString -> [Reader Int B.ByteString] -> Reader Int B.ByteString
sep = undefined

words' :: [Reader Int B.ByteString] -> Reader Int B.ByteString
words' = liftM B.concat . sequence . intersperse (return " ")

instance Concrete Expr where
  concPrec exp = case exp of
    Var v       -> concPrec v
    T           -> return "TRUE"
    F           -> return "FALSE"
    NumLit   n  -> concPrec n
    Not e       -> parens' 10 $ words' [return "NOT", concPrec e]
    And  e0 e1  -> parens' 10 $ words' [concPrec e0, return "AND", concPrec e1]
    Or   e0 e1  -> parens' 10 $ words' [concPrec e0, return "OR" , concPrec e1]
    Impl e0 e1  -> parens' 10 $ words' [concPrec e0, return "=>" , concPrec e1]
    Eq   e0 e1  -> parens' 10 $ words' [concPrec e0, return "="  , concPrec e1]
    Le   e0 e1  -> parens' 10 $ words' [concPrec e0, return "<"  , concPrec e1]
    Leq  e0 e1  -> parens' 10 $ words' [concPrec e0, return "<=" , concPrec e1]
    Ge   e0 e1  -> parens' 10 $ words' [concPrec e0, return ">"  , concPrec e1]
    Geq  e0 e1  -> parens' 10 $ words' [concPrec e0, return ">=" , concPrec e1]
    Add  e0 e1  -> parens' 10 $ words' [concPrec e0, return "+"  , concPrec e1]
    Sub  e0 e1  -> parens' 10 $ words' [concPrec e0, return "-"  , concPrec e1]
    Call f args -> return $ concrete f
                `B.append` ('(' `B.cons` (args' `B.snoc` ')'))
      where
      args' = B.concat $ intersperse ", " (map concrete args)

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
