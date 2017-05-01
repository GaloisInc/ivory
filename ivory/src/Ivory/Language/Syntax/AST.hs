{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Language.Syntax.AST where

import           Prelude                                 ()
import           Prelude.Compat

import           Ivory.Language.Syntax.Concrete.Location
import           Ivory.Language.Syntax.Names
import           Ivory.Language.Syntax.Type

import           Language.Haskell.TH.Lift                (deriveLiftMany)

#if __GLASGOW_HASKELL__ < 709
import           Language.Haskell.TH.Syntax              (Lift (..))
#endif

import           Data.Ratio                              (denominator,
                                                          numerator)

-- Modules ---------------------------------------------------------------------

-- | An external module that defines an imported resource.  A header file in C
-- is an example of this.
type ModulePath = String

data Visible a = Visible
  { public  :: [a]
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
  , modHeaders     :: [FilePath]
    -- ^ Included headers: lists instead of Sets because some systems depend on
    -- a particular header order.
  , modDepends     :: [ModuleName]
    -- ^ Named module dependencies: lists instead of Sets because some systems
    -- depend on a particular header order.
  , modExterns     :: [Extern]
  , modImports     :: [Import]
  , modProcs       :: Visible Proc
  , modStructs     :: Visible Struct
  , modAreas       :: Visible Area
  , modAreaImports :: [AreaImport]
  } deriving (Show, Eq, Ord)

instance Monoid Module where
  mempty = Module
    { modName        = ""
    , modHeaders     = []
    , modDepends     = []
    , modExterns     = []
    , modImports     = []
    , modProcs       = mempty
    , modStructs     = mempty
    , modAreas       = mempty
    , modAreaImports = []
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
    }

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
    -- ^ If-then-else statement. The @Expr@ argument will be typed as an
    -- @IBool@.

  | Assert Expr
    -- ^ Boolean-valued assertions. The @Expr@ argument will be typed as an
    -- @IBool@.

  | CompilerAssert Expr
    -- ^ Compiler-inserted assertion (as opposed to user-level assertions).
    -- These are expected to be correct (e.g., no overflow, etc).  Not exported.

  | Assume Expr
    -- ^ Boolean-valued assumptions. The @Expr@ argument will be typed as an
    -- @IBool@.

  | Return (Typed Expr)
    -- ^ Returning a value.

  | ReturnVoid
    -- ^ Returning void.

  | Deref Type Var Expr
    -- ^ Reference dereferencing. The type parameter refers to the type of the
    -- referenced value, not the reference itself; the expression to be
    -- dereferenced is assumed to always be a reference.

  | Store Type Expr Expr
    -- ^ Storing to a reference. The type parameter refers to the type of the
    -- referenced value, not the reference itself; the expression to be
    -- dereferenced is assumed to always be a reference.

  | Assign Type Var Expr
    -- ^ Simple assignment.

  | Call Type (Maybe Var) Name [Typed Expr]
    -- ^ Function call. The optional variable is where to store the result. It
    -- is expected that the @Expr@ passed for the function symbol will have the
    -- same type as the combination of the types for the arguments, and the
    -- return type.

  | Local Type Var Init
    -- ^ Stack allocation. The type parameter is not a reference at this point;
    -- references are allocated separately to the stack-allocated data.

  | RefCopy Type Expr Expr
    -- ^ Ref copy. Copy the second variable reference to the first (like
    -- memcopy). The type is the dereferenced value of the variables.

  | RefZero Type Expr
    -- ^ Ref zero. Zero out the memory associated with the reference. The type
    -- parameter is not a reference, but the referenced type.

  | AllocRef Type Var Name
    -- ^ Reference allocation. The type parameter is not a reference, but the
    -- referenced type.

  | Loop Integer Var Expr LoopIncr Block
    -- ^ Looping: arguments are the maximum number of iterations of the loop,
    -- loop variable, start value, break condition (for increment or decrement),
    -- and block.

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

-- Imported symbols ------------------------------------------------------------

-- | External Symbols.
data Extern = Extern
  { externSym  :: Sym
  , externFile :: ModulePath
  , externType :: Type
  } deriving (Show, Eq, Ord)

-- Expressions -----------------------------------------------------------------

data Expr
  = ExpSym Sym
    -- ^ Symbols

  | ExpExtern Extern
    -- ^ Imported symbols

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

  | ExpSizeOf Type
    -- ^ Return the allocation size of the given type.

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
  | ExpFAtan2
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

instance Num Expr where
  l * r         = ExpOp ExpMul [l,r]
  l + r         = ExpOp ExpAdd [l,r]
  l - r         = ExpOp ExpSub [l,r]
  abs e         = ExpOp ExpAbs [e]
  signum e      = ExpOp ExpSignum [e]

  negate (ExpLit (LitInteger i)) = ExpLit (LitInteger (negate i))
  negate (ExpLit (LitFloat   f)) = ExpLit (LitFloat   (negate f))
  negate (ExpLit (LitDouble  d)) = ExpLit (LitDouble  (negate d))
  negate e                       = ExpOp ExpNegate [e]

  fromInteger i = ExpLit (LitInteger i)

instance Bounded Expr where
  minBound = ExpMaxMin False
  maxBound = ExpMaxMin True

instance Fractional Expr where
  l / r        = ExpOp ExpDiv [l,r]
  recip a      = ExpOp ExpRecip [a]
  fromRational a = fromInteger (numerator a) / fromInteger (denominator a)

instance Floating Expr where
  pi          = error "pi not implemented for Expr"
  exp e       = ExpOp ExpFExp [e]
  sqrt e      = ExpOp ExpFSqrt [e]
  log e       = ExpOp ExpFLog [e]
  a ** b      = ExpOp ExpFPow [a,b]
  logBase a b = ExpOp ExpFLogBase [a,b]
  sin e       = ExpOp ExpFSin [e]
  tan e       = ExpOp ExpFTan [e]
  cos e       = ExpOp ExpFCos [e]
  asin e      = ExpOp ExpFAsin [e]
  atan e      = ExpOp ExpFAtan [e]
  acos e      = ExpOp ExpFAcos [e]
  sinh e      = ExpOp ExpFSinh [e]
  tanh e      = ExpOp ExpFTanh [e]
  cosh e      = ExpOp ExpFCosh [e]
  asinh e     = ExpOp ExpFAsinh [e]
  atanh e     = ExpOp ExpFAtanh [e]
  acosh e     = ExpOp ExpFAcosh [e]


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
  | InitArray [Init] Bool      -- ^ @ { i1, ..., in } @
  -- Bool true if no unused initialization values.
    deriving (Show, Eq, Ord)


-- TH Lifting ------------------------------------------------------------------

deriveLiftMany
  [ ''Module, ''Visible, ''AreaImport, ''Area, ''Struct
  , ''Import
  , ''Extern
  , ''Proc, ''Ensure, ''Require, ''Cond

  , ''Name
  , ''Stmt, ''LoopIncr, ''Comment, ''SrcLoc, ''Range, ''Position
  , ''Expr, ''ExpOp, ''Literal, ''Init
  ]

#if __GLASGOW_HASKELL__ < 709
instance Lift Double where
  lift = lift . toRational

instance Lift Float where
  lift = lift . toRational
#endif
