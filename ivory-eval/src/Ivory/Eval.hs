{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Ivory.Eval where

import Prelude hiding (negate, div, mod, not, and, or)
import qualified Prelude

import Data.Int
import qualified Data.Map as M
import Data.Word
import MonadLib.Monads
import Ivory.Language.Syntax.Concrete.Location
import qualified Ivory.Language.Syntax as I

-- XXX: DEBUG
import Ivory.Language.Syntax

type Error  = String
type Eval a = StateT EvalState (Exception Error) a

runEval :: M.Map I.Sym Value -> Eval a -> Either Error (a, EvalState)
runEval st runThis = runException (runStateT (initState st) runThis)

data EvalState = EvalState
  { store :: M.Map I.Sym Value
  , loc   :: SrcLoc
  } deriving Show

initState :: M.Map I.Sym Value -> EvalState
initState st = EvalState st NoLoc

data Value
  = Sint8  Int8
  | Uint8  Word8
  | Char   Char
  | String String
  | Bool   Bool
  | Array  [Value]
  | Struct [(I.Sym, Value)]
  | Ref    I.Sym
  deriving (Show, Eq)

eq :: Value -> Value -> Value
eq x y = Bool (x == y)

neq :: Value -> Value -> Value
neq x y = Bool (x /= y)

not :: Value -> Value
not (Bool x) = Bool (Prelude.not x)
not x        = error $ "invalid operands to `not`: " ++ show x

and :: Value -> Value -> Value
and (Bool x) (Bool y) = Bool (x && y)
and x        y        = error $ "invalid operands to `and`: " ++ show (x,y)

or :: Value -> Value -> Value
or (Bool x) (Bool y) = Bool (x || y)
or x        y        = error $ "invalid operors to `or`: " ++ show (x,y)

gt :: Value -> Value -> Value
(Sint8 x) `gt` (Sint8 y) = Bool (x > y)
(Uint8 x) `gt` (Uint8 y) = Bool (x > y)
(Char x)  `gt` (Char y)  = Bool (x > y)
x         `gt` y         = error $ "invalid operands to `gt`: " ++ show (x,y)

gte :: Value -> Value -> Value
(Sint8 x) `gte` (Sint8 y) = Bool (x >= y)
(Uint8 x) `gte` (Uint8 y) = Bool (x >= y)
(Char x)  `gte` (Char y)  = Bool (x >= y)
x         `gte` y         = error $ "invalid operands to `gte`: " ++ show (x,y)

lt :: Value -> Value -> Value
(Sint8 x) `lt` (Sint8 y) = Bool (x < y)
(Uint8 x) `lt` (Uint8 y) = Bool (x < y)
(Char x)  `lt` (Char y)  = Bool (x < y)
x         `lt` y         = error $ "invalid operands to `gt`: " ++ show (x,y)

lte :: Value -> Value -> Value
(Sint8 x) `lte` (Sint8 y) = Bool (x <= y)
(Uint8 x) `lte` (Uint8 y) = Bool (x <= y)
(Char x)  `lte` (Char y)  = Bool (x <= y)
x         `lte` y         = error $ "invalid operands to `lte`: " ++ show (x,y)

add :: Value -> Value -> Value
(Sint8 x) `add` (Sint8 y) = Sint8 (x + y)
(Uint8 x) `add` (Uint8 y) = Uint8 (x + y)
x         `add` y         = error $ "invalid operands to `add`: " ++ show (x,y)

sub :: Value -> Value -> Value
(Sint8 x) `sub` (Sint8 y) = Sint8 (x - y)
(Uint8 x) `sub` (Uint8 y) = Uint8 (x - y)
x         `sub` y         = error $ "invalid operands to `sub`: " ++ show (x,y)

mul :: Value -> Value -> Value
(Sint8 x) `mul` (Sint8 y) = Sint8 (x * y)
(Uint8 x) `mul` (Uint8 y) = Uint8 (x * y)
x         `mul` y         = error $ "invalid operands to `mul`: " ++ show (x,y)

div :: Value -> Value -> Value
(Sint8 x) `div` (Sint8 y) = Sint8 (x `Prelude.div` y)
(Uint8 x) `div` (Uint8 y) = Uint8 (x `Prelude.div` y)
x         `div` y         = error $ "invalid operands to `div`: " ++ show (x,y)

mod :: Value -> Value -> Value
(Sint8 x) `mod` (Sint8 y) = Sint8 (x `Prelude.mod` y)
(Uint8 x) `mod` (Uint8 y) = Uint8 (x `Prelude.mod` y)
x         `mod` y         = error $ "invalid operands to `mod`: " ++ show (x,y)

negate :: Value -> Value
negate (Sint8 x) = Sint8 (Prelude.negate x)
negate (Uint8 x) = Uint8 (Prelude.negate x)
negate x         = error $ "invalid operands to `negate`: " ++ show x

readStore :: I.Sym -> Eval Value
readStore sym = do
  st <- fmap store get
  case M.lookup sym st of
    Nothing -> raise $ "Unbound variable: `" ++ sym ++ "'!"
    Just v  -> return v

writeStore :: I.Sym -> Value -> Eval ()
writeStore sym val = sets_ (\s -> s { store = M.insert sym val (store s) })

modifyStore :: I.Sym -> (Value -> Value) -> Eval ()
modifyStore sym f = sets_ (\s -> s { store = M.update (Just . f) sym (store s) })

updateLoc :: SrcLoc -> Eval ()
updateLoc loc = sets_ (\ s -> s { loc = loc })

----------------------------------------------------------------------
-- | Main Evaluator
----------------------------------------------------------------------
evalBlock :: I.Block -> Eval ()
evalBlock = mapM_ evalStmt

evalStmt :: I.Stmt -> Eval ()
evalStmt stmt = case stmt of
  I.Comment (I.SourcePos loc)
    -> updateLoc loc
  I.Assert expr
    -> evalAssert expr
  I.CompilerAssert expr
    -> evalAssert expr
  I.IfTE cond true false
    -> do b <- evalExpr cond
          case b of
            Bool True  -> evalBlock true
            Bool False -> evalBlock false
            _          -> raise $ "evalStmt: IfTE: expected true or false, got: " ++ show b
  I.Deref _ty var expr
    -> do val <- evalExpr expr
          case val of
            Ref ref -> do
              val <- readStore ref
              writeStore (varSym var) val
            -- _ -> writeStore (varSym var) val
  I.Assign _ty var expr
    -> do val <- evalExpr expr
          writeStore (varSym var) val
  I.Local ty var init
    -> do val <- evalInit ty init
          writeStore (varSym var) val
  I.AllocRef _ty var ref
    -> writeStore (varSym var) (Ref $ nameSym ref)
  I.Loop var expr incr body
    -> do val <- evalExpr expr
          writeStore (varSym var) val
          let (stepFn, doneExpr) = case incr of
                I.IncrTo expr -> ((`add` Sint8 1), expr)
                I.DecrTo expr -> ((`sub` Sint8 1), expr)
          let step = modifyStore (varSym var) stepFn
          let done = do curVal  <- readStore (varSym var)
                        doneVal <- evalExpr doneExpr
                        return (curVal == doneVal)
          untilM done (evalBlock body >> step)
  I.Return (I.Typed  _ty expr)
    -> void $ evalExpr expr
  I.Store _ty (I.ExpVar dst) expr
    -> do val <- evalExpr expr
          Ref var <- readStore (varSym dst)
          writeStore var val

evalAssert :: I.Expr -> Eval ()
evalAssert asrt = do
  b <- evalExpr asrt
  case b of
    Bool True -> return ()
    _         -> raise $ "Assertion failed: " ++ show (b,asrt)

evalExpr :: I.Expr -> Eval Value
evalExpr expr = case expr of
  I.ExpSym sym -> readStore sym
  I.ExpVar var -> readStore (varSym var)
  I.ExpLit lit -> evalLit lit
  I.ExpOp op exprs
    -> do vals <- mapM evalExpr exprs
          evalOp op vals

evalLit :: I.Literal -> Eval Value
evalLit lit = case lit of
  I.LitInteger i -> return (Sint8 $ fromInteger i) -- XXX: fix
  I.LitChar c    -> return (Char c)
  I.LitBool b    -> return (Bool b)
  I.LitString s  -> return (String s)

evalOp :: I.ExpOp -> [Value] -> Eval Value
evalOp (I.ExpEq _)       [x, y] = return (x `eq`  y)
evalOp (I.ExpNeq _)      [x, y] = return (x `neq` y)
evalOp (I.ExpGt True _)  [x, y] = return (x `gte` y)
evalOp (I.ExpGt False _) [x, y] = return (x `gt`  y)
evalOp (I.ExpLt True _)  [x, y] = return (x `lte` y)
evalOp (I.ExpLt False _) [x, y] = return (x `lt`  y)
evalOp I.ExpAdd          [x, y] = return (x `add` y)
evalOp I.ExpSub          [x, y] = return (x `sub` y)
evalOp I.ExpMul          [x, y] = return (x `mul` y)
evalOp I.ExpDiv          [x, y] = return (x `div` y)
evalOp I.ExpMod          [x, y] = return (x `mod` y)
evalOp I.ExpNegate       [x]    = return (negate x)
evalOp I.ExpNot          [x]    = return (not x)
evalOp I.ExpAnd          [x, y] = return (x `and` y)
evalOp I.ExpOr           [x, y] = return (x `or`  y)
evalOp I.ExpCond [cond, true, false] =
  case cond of
    Bool True  -> return true
    Bool False -> return false
    _          -> raise $ "evalStmt: IfTE: expected true or false, got: " ++ show cond



evalInit :: I.Type -> I.Init -> Eval Value
evalInit _ty init = case init of
  I.InitZero ->
    return (Sint8 0)
  I.InitExpr _t expr ->
    evalExpr expr

varSym :: I.Var -> I.Sym
varSym (I.VarName sym)     = sym
varSym (I.VarInternal sym) = sym
varSym (I.VarLitName sym)  = sym

nameSym :: I.Name -> I.Sym
nameSym (I.NameSym sym) = sym
nameSym (I.NameVar var) = varSym var

----------------------------------------------------------------------
-- | Utilities
----------------------------------------------------------------------
untilM :: Monad m => m Bool -> m () -> m ()
untilM done doThis = do
  b <- done
  unless b (doThis >> untilM done doThis)

----------------------------------------------------------------------
-- | Testing
----------------------------------------------------------------------
myloop :: I.Block
myloop =[ Local
            (TyWord Word32)
            (VarName "local0")
            (InitExpr (TyWord Word32) (ExpLit (LitInteger 0)))
        , AllocRef
            (TyWord Word32) (VarName "ref1") (NameVar (VarName "local0"))
        , CompilerAssert
            (ExpOp
               ExpAnd
               [ ExpOp
                   (ExpLt False (TyInt Int32))
                   [ ExpOp ExpSub [ ExpVar (VarName "var0") , ExpLit (LitInteger 1) ]
                   , ExpLit (LitInteger 100)
                   ]
               , ExpOp
                   (ExpLt True (TyInt Int32))
                   [ ExpOp ExpNegate [ ExpLit (LitInteger 1) ]
                   , ExpOp ExpSub [ ExpVar (VarName "var0") , ExpLit (LitInteger 1) ]
                   ]
               ])
        , CompilerAssert
            (ExpOp
               ExpAnd
               [ ExpOp
                   (ExpLt False (TyInt Int32))
                   [ ExpLit (LitInteger 0) , ExpLit (LitInteger 100) ]
               , ExpOp
                   (ExpLt True (TyInt Int32))
                   [ ExpOp ExpNegate [ ExpLit (LitInteger 1) ]
                   , ExpLit (LitInteger 0)
                   ]
               ])
        , Loop
            (VarName "ix2")
            (ExpOp
               ExpCond
               [ ExpOp
                   (ExpLt False (TyInt Int32))
                   [ ExpOp
                       ExpMod
                       [ ExpOp ExpSub [ ExpVar (VarName "var0") , ExpLit (LitInteger 1) ]
                       , ExpLit (LitInteger 100)
                       ]
                   , ExpLit (LitInteger 0)
                   ]
               , ExpOp
                   ExpCond
                   [ ExpOp
                       (ExpGt False (TyInt Int32))
                       [ ExpLit (LitInteger 100) , ExpLit (LitInteger 0) ]
                   , ExpOp
                       ExpAdd
                       [ ExpOp
                           ExpMod
                           [ ExpOp ExpSub [ ExpVar (VarName "var0") , ExpLit (LitInteger 1) ]
                           , ExpLit (LitInteger 100)
                           ]
                       , ExpLit (LitInteger 100)
                       ]
                   , ExpOp
                       ExpSub
                       [ ExpOp
                           ExpMod
                           [ ExpOp ExpSub [ ExpVar (VarName "var0") , ExpLit (LitInteger 1) ]
                           , ExpLit (LitInteger 100)
                           ]
                       , ExpLit (LitInteger 100)
                       ]
                   ]
               , ExpOp
                   ExpMod
                   [ ExpOp ExpSub [ ExpVar (VarName "var0") , ExpLit (LitInteger 1) ]
                   , ExpLit (LitInteger 100)
                   ]
               ])
            (DecrTo
               (ExpOp
                  ExpCond
                  [ ExpOp
                      (ExpLt False (TyInt Int32))
                      [ ExpOp ExpMod [ ExpLit (LitInteger 0) , ExpLit (LitInteger 100) ]
                      , ExpLit (LitInteger 0)
                      ]
                  , ExpOp
                      ExpCond
                      [ ExpOp
                          (ExpGt False (TyInt Int32))
                          [ ExpLit (LitInteger 100) , ExpLit (LitInteger 0) ]
                      , ExpOp
                          ExpAdd
                          [ ExpOp ExpMod [ ExpLit (LitInteger 0) , ExpLit (LitInteger 100) ]
                          , ExpLit (LitInteger 100)
                          ]
                      , ExpOp
                          ExpSub
                          [ ExpOp ExpMod [ ExpLit (LitInteger 0) , ExpLit (LitInteger 100) ]
                          , ExpLit (LitInteger 100)
                          ]
                      ]
                  , ExpOp ExpMod [ ExpLit (LitInteger 0) , ExpLit (LitInteger 100) ]
                  ]))
            [ Deref
                (TyWord Word32) (VarName "deref3") (ExpVar (VarName "ref1"))
            , Store
                (TyWord Word32)
                (ExpVar (VarName "ref1"))
                (ExpOp
                   ExpAdd
                   [ ExpVar (VarName "deref3")
                   , ExpVar (VarName "ix2")
                   -- , ExpSafeCast (TyInt Int32) (ExpVar (VarName "ix2"))
                   ])
            ]
        , Deref
            (TyWord Word32) (VarName "deref4") (ExpVar (VarName "ref1"))
        , Return
            Typed
              { tType = TyWord Word32 , tValue = ExpVar (VarName "deref4") }
        ]
