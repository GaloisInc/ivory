{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.ModelCheck.CVC4 where

import qualified Data.ByteString.Char8 as B
import           Data.Int
import           Data.List             (intersperse)
import           Data.Monoid
import           Data.String
import           Data.Word
import           Prelude               hiding (exp)

import           Ivory.Language.Syntax.Concrete.Location
import           Ivory.Language.Syntax.Concrete.Pretty

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

instance Concrete SrcLoc where
  concrete = concrete . prettyPrint . pretty

data ConcreteList = forall a. Concrete a => CL a

-- Specialization
clBS :: B.ByteString -> ConcreteList
clBS = CL

--------------------------------------------------------------------------------
-- Statements

data Statement = TypeDecl String [(Var, Type)]
               | VarDecl Var Type
               | Assert (Located Expr)
               | Query (Located Expr)
               -- Arbitrary statement constructed by-hand.
               | forall a . Concrete a => Statement a

instance Concrete Statement where
  concrete (TypeDecl ty [])
    = statement [CL ty, clBS ":", clBS "TYPE"]
  concrete (TypeDecl ty fs)
    = statement [CL ty, clBS ":", clBS "TYPE", clBS "= [#", fieldList fs, clBS "#]"]
  concrete (VarDecl v ty)  = statement [CL v, clBS ":", CL ty]
  concrete (Assert (Located loc exp))
    = statement [clBS "ASSERT", CL exp, clBS ";\t %", CL loc]
  concrete (Query (Located loc exp))
    = statement [clBS "QUERY", CL exp, clBS ";\t %", CL loc]
  concrete (Statement a)   = statement [CL a]

statement :: [ConcreteList] -> B.ByteString
statement as =
  let unList (CL a) = concrete a in
  let toks = B.unwords (map unList as) in
  B.snoc toks ';'

fieldList :: [(Var,Type)] -> ConcreteList
fieldList fs = clBS $ B.intercalate ", "
               [concrete v <> " : " <> concrete t | (v,t) <- fs]

typeDecl :: String -> [(Var,Type)] -> Statement
typeDecl = TypeDecl

varDecl :: Var -> Type -> Statement
varDecl = VarDecl

assert :: Located Expr -> Statement
assert = Assert

query :: Located Expr -> Statement
query = Query

--------------------------------------------------------------------------------
-- Expressions and literals

instance Concrete Float where
  concrete = concrete . show

instance Concrete Double where
  concrete = concrete . show

instance Concrete Integer where
  concrete = concrete . show

instance Concrete Int where
  concrete = concrete . show

data Type = Void
          | Integer
          | Real
          | Char
          | Bool
          | Struct String
          | Array Type
          | Opaque
  deriving (Show, Read, Eq)

instance Concrete Type where
  concrete Bool          = "BOOLEAN"
  concrete Real          = "REAL"
  concrete Integer       = "INT"
  concrete (Array t)     = "ARRAY INT OF " <> concrete t
  concrete (Struct name) = B.pack name
  concrete _             = "INT" -- error $ "unexpected type: " ++ show t

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
          | Mod      Expr Integer -- CVC4 can handle mod-by-constant
          | Call     Func [Expr]
          | Store    Expr Expr
          | StoreMany Expr [(Expr,Expr)]
          | Field    Expr Expr
          | Index    Expr Expr

-- Store (Index 4 (Field "bFoo" "var0")) 5
-- var0 WITH .bFoo[4] := 5

-- Index 5 (Index 1) "var0")
-- var0[1][5]

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
  go (Mod x y)   = Mod (go x) y
  go (Call f es) = Call f (map go es)
  go (Store s e) = Store (go s) (go e)
  go (StoreMany a ies) = StoreMany (go a) (map (\(i,e) -> (go i, go e)) ies)
  go (Field f e) = Field (go f) (go e)
  go (Index i e) = Index (go i) (go e)
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
  concrete (Mod e x)     = B.unwords [parens e, "MOD", concrete x]
  concrete (Call f args) = concrete f
                `B.append` ('(' `B.cons` (args' `B.snoc` ')'))
    where
    args' = B.unwords $ intersperse "," (map concrete args)
  concrete (Store s e)   = v <> " WITH " <> f <> " := " <> concrete e
    where
      (v,f) = B.break (`elem` (".[" :: String)) (concrete s)
  -- concrete (Store a i e) = concrete a <> " WITH "
  --                          <> B.concat (map concrete i)
  --                          <> " := " <> concrete e
  concrete (StoreMany a ies)
    = concrete a <> " WITH " <>
      B.intercalate ", " [ f <> " := " <> concrete e
                         | (i,e) <- ies
                         , let f = B.dropWhile (not . (`elem` (".[" :: String)))
                                   (concrete i)
                         ]
  concrete (Field f e)   = concrete e <> "." <> concrete f
  concrete (Index i e)   = concrete e <> "[" <> concrete i <> "]"
  -- concrete (Select e ss) = concrete e <> B.concat (map concrete ss)
  -- concrete (Load a i)    = concrete a <> "[" <> concrete i <> "]"

-- instance Concrete Selector where
--   concrete (Field f) = "." <> concrete f
--   concrete (Index i) = "[" <> concrete i <> "]"

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
(.=>) T e = e
(.=>) x y = Impl x y

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

(.%) :: Expr -> Integer -> Expr
(.%) = Mod

lit :: (Show a, Concrete a, Num a) => a -> Expr
lit = NumLit

intLit :: Integer -> Expr
intLit = lit

realLit :: Double -> Expr
realLit = lit

call :: Func -> [Expr] -> Expr
call = Call

store :: Expr -> Expr -> Expr
store = Store

storeMany :: Expr -> [(Expr,Expr)] -> Expr
storeMany = StoreMany

field :: Expr -> Expr -> Expr
field = Field

index :: Expr -> Expr -> Expr
index = Index

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
  [ B.pack modAbs, ":", "(INT, INT)", "->", "INT" ]

modExp :: Expr -> Expr -> Expr -> Expr
modExp v a b
  =   ((a .>= z) .&& (v .>= z) .&& (v .< b) .&& (v .<= a))
  .|| ((a .< z)  .&& (v .<= z) .&& (v .> b) .&& (v .>= a))
  where
  z = intLit 0


----------------------------------------
-- Mul

mulAbs :: Func
mulAbs = "mul"

mulFunc :: Statement
mulFunc = Statement $ B.unwords
  [ B.pack mulAbs, ":", "(INT, INT)", "->", "INT" ]

mulExp :: Expr -> Expr -> Expr -> Expr
mulExp v a b
  = (((a .== z) .|| (b .== z)) .=> (v .== z))
  .&& ((a .== o) .=> (v .== b))
  .&& ((b .== o) .=> (v .== a))
  .&& (((a .> o) .&& (b .> o)) .=> ((v .> a) .&& (v .> b)))
  where
  z = intLit 0
  o = intLit 1

  
----------------------------------------
-- Div

divAbs :: Func
divAbs = "div"

divFunc :: Statement
divFunc = Statement $ B.unwords
  [ B.pack divAbs, ":", "(INT, INT)", "->", "INT" ]

divExp :: Expr -> Expr -> Expr -> Expr
divExp v a b
  =   ((b .== o) .=> (v .== a))
  .&& ((a .== z) .=> (v .== z))
  .&& (((a .>= o) .&& (b .> o)) .=> ((v .>= z) .&& (v .< a)))
  where
  z = intLit 0
  o = intLit 1


cvc4Lib :: [Statement]
cvc4Lib =
  [ word8Bound, word16Bound, word32Bound, word64Bound
  , int8Bound,  int16Bound,  int32Bound,  int64Bound
  , modFunc, mulFunc, divFunc
  ]

--------------------------------------------------------------------------------
-- Testing

foo :: Statement
foo = assert . noLoc $ (intLit 3 .== var "x") .&& (var "x" .< intLit 4)
