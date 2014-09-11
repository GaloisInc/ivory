--
-- Parser AST.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.Syntax.Concrete.ParseAST where

--------------------------------------------------------------------------------

type FnSym     = String
type Var       = String
type RefVar    = String
type IxVar     = String
type TypeVar   = String
type FieldNm   = String
type MacroVar  = String

--------------------------------------------------------------------------------

-- Top level symbols.
data GlobalSym = GlobalProc ProcDef
               | GlobalStruct StructDef
               | GlobalBitData BitDataDef
               | GlobalTypeDef TypeDef
               | GlobalConstDef ConstDef
               | GlobalInclude IncludeDef
  deriving (Show, Read, Eq, Ord)

--------------------------------------------------------------------------------
-- Includes

data IncludeDef = IncludeDef
  { inclModule :: String
  } deriving (Show, Read, Eq, Ord)

--------------------------------------------------------------------------------
-- Constant definition

data ConstDef = ConstDef
  { constSym :: String
  , constExp :: Exp
  } deriving (Show, Read, Eq, Ord)

--------------------------------------------------------------------------------
-- Type definition

data TypeDef = TypeDef
  { tySym :: String
  , tyDef :: Type
  } deriving (Show, Read, Eq, Ord)

--------------------------------------------------------------------------------
-- Procs

data ProcDef = ProcDef
  { procTy      :: Type         -- ^ Return type
  , procSym     :: FnSym        -- ^ Function name
  , procArgs    :: [(Type,Var)] -- ^ Argument types
  , procStmt    :: [Stmt]       -- ^ Body
  , procPrePost :: [PrePost]
  } deriving (Show, Read, Eq, Ord)

-- Pre and post conditions
data PrePost = PreCond  Exp
             | PostCond Exp
  deriving (Show, Read, Eq, Ord)

--------------------------------------------------------------------------------
-- Types

data Type
  = TyVoid                     -- ^ Unit type
  | TyInt IntSize              -- ^ Signed ints
  | TyWord WordSize            -- ^ Unsigned ints
  | TyBool                     -- ^ Booleans
  | TyChar                     -- ^ Characters
  | TyFloat                    -- ^ Floats
  | TyDouble                   -- ^ Doubles
  | TyRef      Scope AreaTy    -- ^ References
  | TyConstRef Scope AreaTy    -- ^ Constant References
  -- XXX
  -- | TyPtr Type              -- ^ Pointers
  | TyIx Integer               -- ^ Index type
  | TyArea AreaTy              -- ^ Area types
  | TySynonym String           -- ^ Type synonym
  deriving (Show, Read, Eq, Ord)

data AreaTy =
    TyStruct String            -- ^ Structures
  | TyArray AreaTy Integer     -- ^ Arrays of fixed length
  -- XXX
  --  | TyCArray AreaTy        -- ^ C Arrays
  | TyStored Type
  | AreaSynonym String         -- ^ Synonym
  deriving (Show, Read, Eq, Ord)

data Scope =
    Stack (Maybe TypeVar)
    -- ^ Stack allocated.  If no type variable is provided, a fresh one is
    -- constructed.
  | Global
  -- ^ Globally allocated
  | PolyMem (Maybe TypeVar)
  -- ^ Either allocation.  If no type variable is provided, a fresh one is
  -- constructed.
  deriving (Show, Read, Eq, Ord)

data IntSize
  = Int8
  | Int16
  | Int32
  | Int64
  deriving (Show, Read, Eq, Ord)

data WordSize
  = Word8
  | Word16
  | Word32
  | Word64
  deriving (Show, Read, Eq, Ord)

--------------------------------------------------------------------------------
-- Expressions

data Literal
  = LitInteger Integer
  deriving (Show, Read, Eq, Ord)

data Exp
  = ExpLit Literal
  | ExpVar Var
  | ExpRet -- Used only in post-conditions
  | ExpOp ExpOp [Exp]
  | IvoryMacroExp String [Exp]
  | ExpDeref  Exp
  | ExpArray  Exp Exp
  | ExpStruct Exp Exp
  | ExpCall FnSym [Exp]
  deriving (Show, Read, Eq, Ord)

data ExpOp
  = EqOp
  | NeqOp
  | CondOp

  | GtOp Bool
  -- ^ True is >=, False is >
  | LtOp Bool
  -- ^ True is <=, False is <

  | NotOp
  | AndOp
  | OrOp

  | MulOp
  | AddOp
  | SubOp
  | NegateOp
  | AbsOp
  | SignumOp

  | DivOp
  | ModOp

  | FExpOp
  | FSqrtOp
  | FLogOp
  | FPowOp
  | FSinOp
  | FTanOp
  | FCosOp
  | FAsinOp
  | FAtanOp
  | FAcosOp
  | FSinhOp
  | FTanhOp
  | FCoshOp
  | FAsinhOp
  | FAtanhOp
  | FAcoshOp

  | IsNanOp
  | IsInfOp
  | RoundFOp
  | CeilFOp
  | FloorFOp

  | BitAndOp
  | BitOrOp
  | BitXorOp
  | BitComplementOp
  | BitShiftLOp
  | BitShiftROp

  | ConstRefOp

  | SafeCast
  | BitCast
  | CastWith
  | TwosCompCast
  | TwosCompRep

  | ToIx
  | FromIx
  | IxSize
  | ArrayLen
  | ConstRef
  | SizeOf
  | NullPtr
  | RefToPtr
  | ToCArray

  deriving (Show, Read, Eq, Ord)

data AllocRef
  = AllocBase    RefVar (Maybe Exp)
  | AllocArr     RefVar [Exp]
  | AllocStruct  RefVar [(FieldNm, Exp)]
  deriving (Show, Read, Eq, Ord)

-- | AST for parsing C-like statements.
data Stmt
  = IfTE Exp [Stmt] [Stmt]
  | Assert Exp
  | Assume Exp
  | Return Exp
  | ReturnVoid
  -- Deref dereferencing is an expression in our language here.
  | Store Exp Exp
  | Assign Var Exp
  | BindExp Var Exp
  | NoBindCall Var [Exp]
  | RefCopy Exp Exp
-- Local is AllocRef
  | AllocRef AllocRef
  | Loop IxVar [Stmt]
  | Forever [Stmt]
  | IvoryMacroStmt Macro String [Exp]
-- Break XXX Too dangerous (and difficult) for non-macro use?
  deriving (Show, Read, Eq, Ord)

data Macro =
    NoBind
  | Bind MacroVar
  deriving (Show, Read, Eq, Ord)

--------------------------------------------------------------------------------
-- Structs

data StructDef
  = StructDef String [Field]
  | AbstractDef String FilePath
  | StringDef String Integer
    deriving (Show, Read, Eq, Ord)

structSym :: StructDef -> String
structSym s = case s of
  StructDef   sym _ -> sym
  AbstractDef sym _ -> sym
  StringDef   sym _ -> ivoryStringStructName sym

ivoryStringStructName :: String -> String
ivoryStringStructName = ("ivory_string_" ++)

data Field = Field
  { fieldName :: FieldNm
  , fieldType :: AreaTy
  } deriving (Show, Read, Eq, Ord)

--------------------------------------------------------------------------------
-- Bit-data

-- | A "bitdata" definition.
data BitDataDef = BitDataDef
  { defName    :: String
  , defType    :: BitTy
  , defConstrs :: [Constr]
  } deriving (Show, Read, Eq, Ord)

-- | Basic type representation allowed in bit definitions.
data BitTy = Bit
           | Bits Integer
           | BitArray Integer BitTy
           | BitTySynonym String
  deriving (Show, Read, Eq, Ord)

-- | A constructor definition within a "bitdata".
data Constr = Constr
  { constrName   :: String
  , constrFields :: [BitField]
  , constrLayout :: [LayoutItem]
  } deriving (Show, Read, Eq, Ord)

-- | One element of a bit data constructor layout.
data LayoutItem = LayoutConst BitLiteral
                | LayoutField String
  deriving (Show, Read, Eq, Ord)

-- | A bit integer literal with a known or unknown size.
data BitLiteral =
    BitLitKnown   { bitLitLen :: Integer , bitLitVal :: Integer }
  | BitLitUnknown { bitLitVal :: Integer }
 deriving (Show, Read, Eq, Ord)

-- | A record-like field defined within a "bitdata" constructor.  If the name is
-- an underscore, we name it with 'Nothing'.
data BitField = BitField
  { bitFieldName :: Maybe String
  , bitFieldType :: BitTy
  } deriving (Show, Read, Eq, Ord)

--------------------------------------------------------------------------------
