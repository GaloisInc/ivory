{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Language.Syntax.AST where

import Ivory.Language.Syntax.Concrete.Location
import Ivory.Language.Syntax.Names
import Ivory.Language.Syntax.Type

import Data.Monoid (Monoid(..))
import Language.Haskell.TH.Lift (deriveLiftMany)
import Language.Haskell.TH.Syntax (Lift(..))

import Data.Ratio (denominator, numerator)
import qualified Data.Set as Set


-- Modules ---------------------------------------------------------------------

-- | An external module that defines an imported resource.  A header file in C
-- is an example of this.
type ModulePath = String

data Visible a = Visible
  { public :: [a]
  , private :: [a]
  } deriving (Show, Eq, Ord)

instance Monoid (Visible a) where
  mempty                                  = Visible [] []
  mappend (Visible l0 l1) (Visible m0 m1) = Visible (l0 ++ m0) (l1 ++ m1)

-- | The name of a module defined in Ivory.
type ModuleName = String

data Module = Module
  { modName        :: ModuleName
    -- ^ The name of this module
  , modHeaders     :: Set.Set FilePath
    -- ^ Included headers
  , modDepends     :: Set.Set ModuleName
    -- ^ Named module dependencies
  , modExterns     :: [Extern]
  , modImports     :: [Import]
  , modProcs       :: Visible Proc
  , modStructs     :: Visible Struct
  , modAreas       :: Visible Area
  , modAreaImports :: [AreaImport]
  , modSourceDeps  :: Set.Set FilePath
  } deriving (Show, Eq, Ord)

instance Monoid Module where
  mempty = Module
    { modName        = ""
    , modHeaders     = Set.empty
    , modDepends     = Set.empty
    , modExterns     = []
    , modImports     = []
    , modProcs       = mempty
    , modStructs     = mempty
    , modAreas       = mempty
    , modAreaImports = []
    , modSourceDeps  = Set.empty
    }

  mappend l r = Module
    { modName        = modName (if null (modName l) then r else l)
    , modHeaders     = modHeaders     l `mappend` modHeaders     r
    , modDepends     = modDepends     l `mappend` modDepends     r
    , modExterns     = modExterns     l `mappend` modExterns     r
    , modImports     = modImports     l `mappend` modImports     r
    , modProcs       = modProcs       l `mappend` modProcs       r
    , modStructs     = modStructs     l `mappend` modStructs     r
    , modAreas       = modAreas       l `mappend` modAreas       r
    , modAreaImports = modAreaImports l `mappend` modAreaImports r
    , modSourceDeps  = modSourceDeps  l `mappend` modSourceDeps  r
    }


-- External Functions ----------------------------------------------------------

-- | Functions not defined in a header, but are available to the linker.
data Extern = Extern
  { externSym     :: Sym
  , externRetType :: Type
  , externArgs    :: [Type]
  } deriving (Show, Eq, Ord)


-- Imported Functions ----------------------------------------------------------

-- | Functions that are defined in a c header.
data Import = Import
  { importSym      :: Sym
  , importFile     :: ModulePath
  , importRetTy    :: Type
  , importArgs     :: [Typed Var]
  , importRequires :: [Require]
  , importEnsures  :: [Ensure]
  } deriving (Show, Eq, Ord)


-- Procedures ------------------------------------------------------------------

-- | Functions defined in the language.
data Proc = Proc
  { procSym      :: Sym
  , procRetTy    :: Type
  , procArgs     :: [Typed Var]
  , procBody     :: Block
  , procRequires :: [Require]
  , procEnsures  :: [Ensure]
  } deriving (Show, Eq, Ord)


-- Structure Definitions -------------------------------------------------------

data Struct
  = Struct String [Typed String]
  | Abstract String ModulePath
    deriving (Show, Eq, Ord)

structName :: Struct -> String
structName def = case def of
  Struct n _   -> n
  Abstract n _ -> n


-- Global Memory Areas ---------------------------------------------------------

data Area = Area
  { areaSym   :: Sym
  , areaConst :: Bool
  , areaType  :: Type
  , areaInit  :: Init
  } deriving (Show, Eq, Ord)


-- Imported Memory Areas -------------------------------------------------------

data AreaImport = AreaImport
  { aiSym   :: Sym
  , aiConst :: Bool
  , aiFile  :: ModulePath
  } deriving (Show, Eq, Ord)


-- Statements ------------------------------------------------------------------

type Block = [Stmt]

data Stmt
  = IfTE Expr Block Block
    -- ^ If-then-else statement.  The @Expr@ argument will be typed as an
    -- @IBool@.

  | Assert Expr
    -- ^ Boolean-valued assertions.  The @Expr@ argument will be typed as an
    -- @IBool@.

  | CompilerAssert Expr
    -- ^ Compiler-inserted assertion (as opposed to user-level assertions).
    -- These are expected to be correct (e.g., no overflow, etc).  Not exported.

  | Assume Expr
    -- ^ Boolean-valued assumptions.  The @Expr@ argument will be typed as an
    -- @IBool@.

  | Return (Typed Expr)
    -- ^ Returning a value.

  | ReturnVoid
    -- ^ Returning void.

  | Deref Type Var Expr
    -- ^ Reference dereferencing.  The type parameter refers to the type of the
    -- referenced value, not the reference itself; the expression to be
    -- dereferenced is assumed to always be a reference.

  | Store Type Expr Expr
    -- ^ Storing to a reference.  The type parameter refers to the type of the
    -- referenced value, not the reference itself; the expression to be
    -- dereferenced is assumed to always be a reference.

  | Assign Type Var Expr
    -- ^ Simple assignment.

  | Call Type (Maybe Var) Name [Typed Expr]
    -- ^ Function call.  The optional variable is where to store the result.  It
    -- is expected that the @Expr@ passed for the function symbol will have the
    -- same type as the combination of the types for the arguments, and the
    -- return type.

  | Local Type Var Init
    -- ^ Stack allocation.  The type parameter is not a reference at this point;
    -- references are allocated separately to the stack-allocated data.

  | RefCopy Type Expr Expr
    -- ^ Ref copy.  Copy the second variable reference to the fist (like
    -- memcopy).  The type is the dereferenced value of the variables.

  | AllocRef Type Var Name
    -- ^ Reference allocation.  The type parameter is not a reference, but the
    -- referenced type.

  | Loop Var Expr LoopIncr Block
    -- ^ Looping: arguments are the loop variable, start value,
    -- break condition (for increment or decrement), and block.

  | Forever Block
    -- ^ Nonterminting loop

  | Break
    -- ^ Break out of a loop

  | Comment Comment
    -- ^ User comment, can be used to output a comment in the backend.
    deriving (Show, Eq, Ord)

data LoopIncr
  = IncrTo Expr
  | DecrTo Expr
    deriving (Show, Eq, Ord)

data Name
  = NameSym Sym
  | NameVar Var
    deriving (Show, Eq, Ord)

data Comment = UserComment String
             | SourcePos   SrcLoc
               deriving (Show, Eq, Ord)


-- Conditions ------------------------------------------------------------------

data Cond
  = CondBool Expr
    -- ^ Boolean Expressions

  | CondDeref Type Expr Var Cond
    -- ^ Dereference introduction.  The type is the type of the dereferenced
    -- thing, not the reference itself.
    deriving (Show, Eq, Ord)


-- Pre-conditions --------------------------------------------------------------

newtype Require = Require
  { getRequire :: Cond
  } deriving (Show, Eq, Ord)


-- Post-conditions -------------------------------------------------------------

-- | Ensure statements describe properties of the return value for the function
-- they annotate.  The return value is referenced through the special internal
-- variable, "retval".
newtype Ensure = Ensure
  { getEnsure :: Cond
  } deriving (Show, Eq, Ord)


-- Expressions -----------------------------------------------------------------

data Expr
  = ExpSym Sym
    -- ^ Symbols

  | ExpVar Var
    -- ^ Variables

  | ExpLit Literal
    -- ^ Literals

  | ExpLabel Type Expr String
    -- ^ Struct label indexing.

  | ExpIndex Type Expr Type Expr -- XXX Do we need the 2nd (index) Type?
    -- ^ Array indexing.  The type is the type of the array being indexed, it's
    -- implied that the expression with the array in it is a reference.

  | ExpToIx Expr Integer
    -- ^ Cast from an expression to an index (Ix) used in loops and array
    -- indexing.  The Integer is the maximum bound.

  | ExpSafeCast Type Expr
    -- ^ Type-safe casting.  The type is the type casted from.

  | ExpOp ExpOp [Expr]
    -- ^ Primitive expression operators

  | ExpAddrOfGlobal Sym
    -- ^ Take the address of a global memory area, introduced through a MemArea
    -- *only*.

  | ExpMaxMin Bool
    -- ^ True is max value, False is min value for the type.

    deriving (Show, Eq, Ord)


-- Expression Operators --------------------------------------------------------

data ExpOp
  = ExpEq Type
  | ExpNeq Type
  | ExpCond

  | ExpGt Bool Type
  -- ^ True is >=, False is >
  | ExpLt Bool Type
  -- ^ True is <=, False is <

  | ExpNot
  | ExpAnd
  | ExpOr

  | ExpMul
  | ExpAdd
  | ExpSub
  | ExpNegate
  | ExpAbs
  | ExpSignum

  | ExpDiv
  | ExpMod
  | ExpRecip

  | ExpFExp
  | ExpFSqrt
  | ExpFLog
  | ExpFPow
  | ExpFLogBase
  | ExpFSin
  | ExpFTan
  | ExpFCos
  | ExpFAsin
  | ExpFAtan
  | ExpFAcos
  | ExpFSinh
  | ExpFTanh
  | ExpFCosh
  | ExpFAsinh
  | ExpFAtanh
  | ExpFAcosh

  | ExpIsNan Type
  | ExpIsInf Type
  | ExpRoundF
  | ExpCeilF
  | ExpFloorF

  | ExpBitAnd
  | ExpBitOr
  | ExpBitXor
  | ExpBitComplement
  | ExpBitShiftL
  | ExpBitShiftR

    deriving (Show, Eq, Ord)

isLiteralValue :: Integer -> Literal -> Bool
isLiteralValue v (LitInteger i) = v == i
isLiteralValue v (LitFloat f) = fromInteger v == f
isLiteralValue v (LitDouble f) = fromInteger v == f
isLiteralValue _ _ = False

getInteger :: Literal -> Maybe Integer
getInteger (LitInteger i) = Just i
getInteger _ = Nothing

getDouble :: Literal -> Maybe Double
getDouble (LitInteger i) = Just $ fromInteger i
getDouble (LitFloat f) = Just $ realToFrac f
getDouble (LitDouble d) = Just d
getDouble _ = Nothing

unOpFloat :: ExpOp -> (Double -> Double) -> Expr -> Expr
unOpFloat _ f (ExpLit (getDouble -> Just v)) = ExpLit $ LitDouble $ f v
unOpFloat o _ v = ExpOp o [v]

unOpNum :: ExpOp -> (Integer -> Integer) -> (Double -> Double) -> Expr -> Expr
unOpNum _ f _ (ExpLit (getInteger -> Just v)) = ExpLit $ LitInteger $ f v
unOpNum o _ d v = unOpFloat o d v

binOpFloat :: ExpOp -> (Double -> Double -> Double) -> Expr -> Expr -> Expr
binOpFloat _ f (ExpLit (getDouble -> Just l)) (ExpLit (getDouble -> Just r)) = ExpLit $ LitDouble $ f l r
binOpFloat o _ l r = ExpOp o [l,r]

binOpNum :: ExpOp -> (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> Expr -> Expr -> Expr
binOpNum _ f _ (ExpLit (getInteger -> Just l)) (ExpLit (getInteger -> Just r)) = ExpLit $ LitInteger $ f l r
binOpNum o _ d l r = binOpFloat o d l r

instance Num Expr where
  l * (ExpLit r)
    | isLiteralValue 0 r = ExpLit r
    | isLiteralValue 1 r = l
    | isLiteralValue (-1) r = negate l
  l * (ExpOp ExpRecip [r]) = l / r
  (ExpLit l) * r
    | isLiteralValue 0 l = ExpLit l
    | isLiteralValue 1 l = r
    | isLiteralValue (-1) l = negate r
  (ExpOp ExpRecip [l]) * r = r / l
  l * r         = binOpNum ExpMul (*) (*) l r

  l + (ExpLit r) | isLiteralValue 0 r = l
  l + (ExpOp ExpNegate [r]) = l - r
  (ExpLit l) + r | isLiteralValue 0 l = r
  (ExpOp ExpNegate [l]) + r = r - l
  l + r         = binOpNum ExpAdd (+) (+) l r

  l - (ExpLit r) | isLiteralValue 0 r = l
  l - (ExpOp ExpNegate [r]) = l + r
  (ExpLit l) - r | isLiteralValue 0 l = negate r
  (ExpOp ExpNegate [l]) - r = negate $ l + r
  l - r         = binOpNum ExpSub (-) (-) l r

  abs e         = unOpNum ExpAbs abs abs e
  signum e      = unOpNum ExpSignum signum signum e
  negate (ExpOp ExpNegate [e]) = e
  negate e      = unOpNum ExpNegate negate negate e
  fromInteger i = ExpLit (LitInteger i)

instance Bounded Expr where
  minBound = ExpMaxMin False
  maxBound = ExpMaxMin True

instance Fractional Expr where
  l / (ExpLit r) | isLiteralValue 1 r = l
  (ExpLit l) / r | isLiteralValue 1 l = recip r
  l / r        = binOpFloat ExpDiv (/) l r
  recip a      = unOpFloat ExpRecip recip a
  fromRational a = fromInteger (numerator a) / fromInteger (denominator a)

instance Floating Expr where
  pi          = error "pi not implemented for Expr"
  exp e       = unOpFloat ExpFExp exp e
  sqrt e      = unOpFloat ExpFSqrt sqrt e
  log e       = unOpFloat ExpFLog log e
  a ** b      = binOpFloat ExpFPow (**) a b
  logBase a b = binOpFloat ExpFLogBase logBase a b
  sin e       = unOpFloat ExpFSin sin e
  tan e       = unOpFloat ExpFTan tan e
  cos e       = unOpFloat ExpFCos cos e
  asin e      = unOpFloat ExpFAsin asin e
  atan e      = unOpFloat ExpFAtan atan e
  acos e      = unOpFloat ExpFAcos acos e
  sinh e      = unOpFloat ExpFSinh sinh e
  tanh e      = unOpFloat ExpFTanh tanh e
  cosh e      = unOpFloat ExpFCosh cosh e
  asinh e     = unOpFloat ExpFAsinh asinh e
  atanh e     = unOpFloat ExpFAtanh atanh e
  acosh e     = unOpFloat ExpFAcosh acosh e


-- Literals --------------------------------------------------------------------

data Literal
  = LitInteger Integer
  | LitFloat Float
  | LitDouble Double
  | LitChar Char
  | LitBool Bool
  | LitNull
  | LitString String
    deriving (Show, Eq, Ord)


-- Initializers ----------------------------------------------------------------

-- | An initializer with no 'InitExpr' fields corresponds to @{0}@.
zeroInit :: Init
zeroInit  = InitZero

data Init
  = InitZero                   -- ^ @ {} @
  | InitExpr Type Expr         -- ^ @ expr @
  | InitStruct [(String,Init)] -- ^ @ { .f1 = i1, ..., .fn = in } @
  | InitArray [Init]           -- ^ @ { i1, ..., in } @
    deriving (Show, Eq, Ord)


-- TH Lifting ------------------------------------------------------------------

deriveLiftMany
  [ ''Module, ''Visible, ''AreaImport, ''Area, ''Struct
  , ''Import
  , ''Proc, ''Ensure, ''Require, ''Cond
  , ''Extern, ''Set.Set

  , ''Name
  , ''Stmt, ''LoopIncr, ''Comment, ''SrcLoc, ''Range, ''Position
  , ''Expr, ''ExpOp, ''Literal, ''Init
  ]

instance Lift Double where
  lift = lift . toRational

instance Lift Float where
  lift = lift . toRational
