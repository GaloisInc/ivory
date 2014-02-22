module Ivory.Language.CSyntax.ParseAST where

--------------------------------------------------------------------------------

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
  = AddOp
  deriving (Eq, Show, Read)

data AllocRef
  = AllocBase RefVar Exp
  | AllocArr  RefVar [Exp]
  deriving (Eq, Show, Read)

-- | AST for parsing C-like statements.
data Stmt
  = IfTE Exp [Stmt] [Stmt]
    -- ^ if (exp) { stmts } else { stmts }
--  | Assert
--  | CompilerAssert
--  | Assume
  | Return Exp
    -- ^ return exp;
  | ReturnVoid
    -- ^ return;
--  | Deref XXX dereferencing is an expression in our language here.
  | Store RefLVal Exp
    -- ^ * var = exp;
  | Assign Var Exp
    -- ^ var = exp;
--  | Call
--  | Local
--  | RefCopy
  | AllocRef AllocRef
    -- ^ * var = init;
    -- ^ arr[] = {0,1,2};
  | Loop IxVar [Stmt]
--  | Forever
--  | Break
  deriving (Eq, Show, Read)
