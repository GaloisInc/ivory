--
-- Parser AST.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.Syntax.Concrete.ParseAST where

import Data.Monoid

import Ivory.Language.Syntax.Concrete.Location

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
  , inclDefLoc :: SrcLoc
  } deriving (Show, Read, Eq, Ord)

--------------------------------------------------------------------------------
-- Constant definition

data ConstDef = ConstDef
  { constSym    :: String
  , constExp    :: Exp
  , constType   :: Maybe Type
  , constDefLoc :: SrcLoc
  } deriving (Show, Read, Eq, Ord)

--------------------------------------------------------------------------------
-- Type definition

data TypeDef = TypeDef
  { tySym    :: String
  , tyDef    :: Type
  , tyDefLoc :: SrcLoc
  } deriving (Show, Read, Eq, Ord)

--------------------------------------------------------------------------------
-- Procs

data ProcDef = ProcDef
  { procTy      :: Type         -- ^ Return type
  , procSym     :: FnSym        -- ^ Function name
  , procArgs    :: [(Type,Var)] -- ^ Argument types
  , procStmt    :: [Stmt]       -- ^ Body
  , procPrePost :: [PrePost]
  , procLoc     :: SrcLoc
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
  -- XXX
  -- | TyPtr Type              -- ^ Pointers
  | TyIx Integer               -- ^ Index type
  | TyStored Type              -- ^ References
  | TyStruct String            -- ^ Structures
  | TyArray Type Integer       -- ^ Arrays of fixed lignth
  | TyRef      Scope Type      -- ^ References
  | TyConstRef Scope Type      -- ^ Constant References
  | TySynonym String           -- ^ Type synonym
  | LocTy (Located Type)
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
  | IvoryMacroExp (String,[Exp])
  | ExpDeref  Exp
  | ExpArray  Exp Exp
  | ExpStruct Exp Exp
  | ExpCall FnSym [Exp]
  | ExpAddrOf Var
  | LocExp (Located Exp)
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
  | Assign Var Exp (Maybe Type)
  | NoBindCall Var [Exp]
  | RefCopy Exp Exp
-- Local is AllocRef
  | AllocRef AllocRef
  | MapArr IxVar [Stmt]
  | UpTo Exp IxVar [Stmt]
  | Forever [Stmt]
  | IvoryMacroStmt (Maybe Var) (String, [Exp])
-- Break XXX Too dangerous (and difficult) for non-macro use?
  | LocStmt (Located Stmt)
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
  , fieldType :: Type
  , fieldLoc  :: SrcLoc
  } deriving (Show, Read, Eq, Ord)

--------------------------------------------------------------------------------
-- Bit-data

-- | A "bitdata" definition.
data BitDataDef = BitDataDef
  { defName    :: String
  , defType    :: BitTy
  , defConstrs :: [Constr]
  , bdLoc      :: SrcLoc
  } deriving (Show, Read, Eq, Ord)

-- | Basic type representation allowed in bit definitions.
data BitTy = Bit
           | Bits Integer
           | BitArray Integer BitTy
           | BitTySynonym String
           | LocBitTy (Located BitTy)
  deriving (Show, Read, Eq, Ord)

-- | A constructor definition within a "bitdata".
data Constr = Constr
  { constrName   :: String
  , constrFields :: [BitField]
  , constrLayout :: [LayoutItem]
  , constrLoc    :: SrcLoc
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
  , bitFieldLoc  :: SrcLoc
  } deriving (Show, Read, Eq, Ord)

--------------------------------------------------------------------------------
-- Instances

instance HasLocation GlobalSym where
  getLoc = mempty
  stripLoc g = case g of
    GlobalProc p     -> GlobalProc (stripLoc p)
    GlobalStruct s   -> GlobalStruct (stripLoc s)
    GlobalBitData b  -> GlobalBitData (stripLoc b)
    GlobalTypeDef t  -> GlobalTypeDef (stripLoc t)
    GlobalConstDef c -> GlobalConstDef (stripLoc c)
    GlobalInclude i  -> GlobalInclude (stripLoc i)

instance HasLocation IncludeDef where
  getLoc = inclDefLoc
  stripLoc incl = incl { inclDefLoc = mempty }

instance HasLocation ConstDef where
  getLoc = constDefLoc
  stripLoc c = c { constDefLoc = mempty }

instance HasLocation TypeDef where
  getLoc = tyDefLoc
  stripLoc td = td { tyDefLoc = mempty }

instance HasLocation ProcDef where
  getLoc = procLoc
  stripLoc p = p { procLoc = mempty }

instance HasLocation PrePost where
  getLoc _ = mempty
  stripLoc pp = case pp of
    PreCond e  -> PreCond (stripLoc e)
    PostCond e -> PostCond (stripLoc e)

instance HasLocation Type where
  getLoc ty = case ty of
                LocTy t -> getLoc t
                _       -> mempty
  stripLoc ty = case ty of
    TyVoid{}         -> ty
    TyInt{}          -> ty
    TyWord{}         -> ty
    TyBool{}         -> ty
    TyChar{}         -> ty
    TyFloat{}        -> ty
    TyDouble{}       -> ty
    TyIx{}           -> ty
    TyStored ty0     -> TyStored (stripLoc ty0)
    TyStruct{}       -> ty
    TyArray ty0 i    -> TyArray (stripLoc ty0) i
    TyRef s ty0      -> TyRef s (stripLoc ty0)
    TyConstRef s ty0 -> TyConstRef s (stripLoc ty0)
    TySynonym{}      -> ty
    LocTy ty0        -> unLoc ty0

instance HasLocation Exp where
  getLoc e = case e of
               LocExp le -> getLoc le
               _         -> mempty

  stripLoc e = case e of
    ExpLit{}               -> e
    ExpVar{}               -> e
    ExpRet{}               -> e
    ExpOp op args          -> ExpOp op (stripLoc args)
    IvoryMacroExp (s,args) -> IvoryMacroExp (s, stripLoc args)
    ExpDeref e0            -> ExpDeref (stripLoc e0)
    ExpArray e0 e1         -> ExpArray (stripLoc e0) (stripLoc e1)
    ExpStruct e0 e1        -> ExpStruct (stripLoc e0) (stripLoc e1)
    ExpCall fn args        -> ExpCall fn (stripLoc args)
    ExpAddrOf{}            -> e
    LocExp le              -> unLoc le

instance HasLocation AllocRef where
  getLoc _ = mempty
  stripLoc a = case a of
    AllocBase v e    -> AllocBase v (stripLoc e)
    AllocArr v es    -> AllocArr v (stripLoc es)
    AllocStruct v fs -> AllocStruct v (map (\(n,e) -> (n, stripLoc e)) fs)

instance HasLocation Stmt where
  getLoc s = case s of
               LocStmt s0 -> getLoc s0
               _          -> mempty
  stripLoc s = case s of
    IfTE e s0 s1    -> IfTE (stripLoc e) (stripLoc s0) (stripLoc s1)
    Assert e        -> Assert (stripLoc e)
    Assume e        -> Assume (stripLoc e)
    Return e        -> Return (stripLoc e)
    ReturnVoid      -> ReturnVoid
    Store e0 e1     -> Store (stripLoc e0) (stripLoc e1)
    Assign v e t    -> Assign v (stripLoc e) (stripLoc t)
    NoBindCall v es -> NoBindCall v (stripLoc es)
    RefCopy e0 e1   -> RefCopy (stripLoc e0) (stripLoc e1)
    AllocRef ar     -> AllocRef (stripLoc ar)
    MapArr v ss     -> MapArr v (stripLoc ss)
    UpTo e v ss     -> UpTo (stripLoc e) v (stripLoc ss)
    Forever ss      -> Forever (stripLoc ss)
    IvoryMacroStmt v (s0,es) -> IvoryMacroStmt v (s0, stripLoc es)
    LocStmt s0      -> unLoc s0

instance HasLocation StructDef where
  getLoc _ = mempty
  stripLoc s = case s of
    StructDef s0 fs  -> StructDef s0 (stripLoc fs)
    AbstractDef{}    -> s
    StringDef{}      -> s

instance HasLocation Field where
  getLoc = fieldLoc
  stripLoc (Field n t _) = Field n (stripLoc t) mempty

instance HasLocation BitDataDef where
  getLoc = bdLoc
  stripLoc (BitDataDef s t cs _) = BitDataDef s (stripLoc t) (stripLoc cs) mempty

instance HasLocation BitTy where
  getLoc bt = case bt of
                LocBitTy bt' -> getLoc bt'
                _            -> mempty
  stripLoc bt = case bt of
    Bit            -> bt
    Bits{}         -> bt
    BitArray i bt0 -> BitArray i (stripLoc bt0)
    BitTySynonym{} -> bt
    LocBitTy bt0   -> unLoc bt0

instance HasLocation Constr where
  getLoc = constrLoc
  stripLoc (Constr n fs l _) = Constr n (stripLoc fs) l mempty

instance HasLocation BitField where
  getLoc = bitFieldLoc
  stripLoc (BitField n t _) = BitField n (stripLoc t) mempty

