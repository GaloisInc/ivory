module Ivory.Language.CSyntax.ParseAST where

--------------------------------------------------------------------------------

type FnSym     = String
type Var       = String
type RefVar    = String
type IxVar     = String
--type Size      = Integer

data RefLVal
  = RefVar RefVar
  | ArrIx RefVar Exp
  deriving (Eq, Show, Read)

data Literal
  = LitInteger Integer
  deriving (Eq, Show, Read)

data Exp
  = ExpLit Literal
  | ExpVar Var
  | ExpDeref RefVar -- Note: these are statements in Ivory.  We constrain the
                 -- language here: you can only deref a RefVar.
  | ExpOp ExpOp [Exp]
  | ExpArrIx RefVar Exp
  | ExpAnti String
    -- ^ Ivory antiquotation
  deriving (Eq, Show, Read)

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
-- Don't need in language
--  | RecipOp

  | FExpOp
  | FSqrtOp
  | FLogOp
  | FPowOp
-- Don't need in language
--  | FLogBaseOp
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

  deriving (Eq, Show, Read)

data AllocRef
  = AllocBase RefVar Exp
  | AllocArr  RefVar [Exp]
  deriving (Eq, Show, Read)

-- | AST for parsing C-like statements.
data Stmt
  = IfTE Exp [Stmt] [Stmt]
  | Assert Exp
  | Assume Exp
  | Return Exp
  | ReturnVoid
--  | Deref dereferencing is an expression in our language here.
  | Store RefLVal Exp
  | Assign Var Exp
  | Call (Maybe Var) FnSym [Exp]
  | RefCopy Exp Exp
-- Local is AllocRef
  | AllocRef AllocRef
  | Loop IxVar [Stmt]
  | Forever [Stmt]
--  | Break Too dangerous (and difficult) for non-macro use?
  deriving (Eq, Show, Read)
