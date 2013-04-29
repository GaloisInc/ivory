module Ivory.Opts.PreProc where

import qualified Ivory.Language.Proc as P
import qualified Ivory.Language.Syntax.AST as I
import qualified Ivory.Language.Syntax.Type as I

--------------------------------------------------------------------------------
-- Expression traversal.
--------------------------------------------------------------------------------

-- | Expression to expression optimization.
type ExprOpt = I.Expr -> I.Expr

-- | Optimize a single definition.
defFold :: ExprOpt -> P.Def a -> P.Def a
defFold opt def =
  case def of
    P.DefProc fun     -> P.DefProc (procFold opt fun)
    P.DefExtern{}     -> def
    P.DefImport{}     -> def

procFold :: ExprOpt -> I.Proc -> I.Proc
procFold opt proc =
  let body' = map (stmtFold opt) (I.procBody proc) in
  proc { I.procBody = body' }

stmtFold :: ExprOpt -> I.Stmt -> I.Stmt
stmtFold opt stmt =
  case stmt of
    I.IfTE e b0 b1       -> I.IfTE (opt e) (map sf b0) (map sf b1)
    I.Assert e           -> I.Assert (opt e)
    I.Return e           -> I.Return (typedFold opt e)
    I.ReturnVoid         -> I.ReturnVoid
    I.Deref t var e      -> I.Deref t var (opt e)
    I.Store t e0 e1      -> I.Store t (opt e0) (opt e1)
    I.Assign t v e       -> I.Assign t v (opt e)
    I.Call t mv c tys    -> I.Call t mv c (map (typedFold opt) tys)
    I.Local{}            -> stmt
    I.RefCopy t e0 e1    -> I.RefCopy t (opt e0) (opt e1)
    I.AllocRef{}         -> stmt
    I.Loop ty v e incr b -> I.Loop ty v (opt e) (loopIncrFold opt incr)
                              (map sf b)
    I.Break              -> I.Break
    I.Forever b          -> I.Forever (map sf b)
  where sf = stmtFold opt

loopIncrFold :: ExprOpt -> I.LoopIncr -> I.LoopIncr
loopIncrFold opt incr =
  case incr of
    I.IncrTo e0 -> I.IncrTo (opt e0)
    I.DecrTo e0 -> I.DecrTo (opt e0)

typedFold :: ExprOpt -> I.Typed I.Expr -> I.Typed I.Expr
typedFold opt ty = ty { I.tValue = opt (I.tValue ty) }
