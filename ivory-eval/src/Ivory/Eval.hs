{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | A simple interpreter for a subset of Ivory.
module Ivory.Eval
  ( runEval
  , runEvalStartingFrom
  , Error
  , Eval
  , EvalState(EvalState)
  , Value(..)
  , initState
  , openModule
  , evalAssert
  , evalBlock
  , evalCond
  , evalDeref
  , evalExpr
  , evalInit
  , evalLit
  , evalOp
  , evalRequires
  , evalStmt
  ) where

import           Prelude                                 ()
import           Prelude.Compat                          hiding (and, div, mod,
                                                          negate, not, or)
import qualified Prelude.Compat                          as Prelude


import           Control.Monad                           (foldM, unless, void)
import           Data.Int
import qualified Data.Map                                as Map
import           Data.Maybe
import qualified Data.Sequence                           as Seq
import           Data.Word
import qualified Ivory.Language.Array                    as I
import qualified Ivory.Language.Syntax                   as I
import           Ivory.Language.Syntax.Concrete.Location
import           MonadLib                                (ExceptionM (..),
                                                          ExceptionT, Id,
                                                          StateM (..), StateT,
                                                          runExceptionT, runId,
                                                          runStateT, sets_)

-- XXX: DEBUG
-- import Debug.Trace

type Error  = String
type Eval a = StateT EvalState (ExceptionT Error Id) a

runEval :: Eval a -> Either Error a
runEval doThis = fmap fst (runEvalStartingFrom (initState Map.empty) doThis)

runEvalStartingFrom :: EvalState -> Eval a -> Either Error (a, EvalState)
runEvalStartingFrom st doThis = runId (runExceptionT (runStateT st doThis))

data EvalState = EvalState
  { store   :: Map.Map I.Sym Value
  , loc     :: SrcLoc
  , structs :: Map.Map I.Sym I.Struct
  } deriving Show

initState :: Map.Map I.Sym Value -> EvalState
initState st = EvalState st NoLoc Map.empty

-- | Run an action inside the scope of a given module.
openModule :: I.Module -> Eval a -> Eval a
openModule (I.Module {..}) doThis = do
  oldStrs <- fmap structs get
  sets_ (\s -> s { structs = Map.union (structs s) newStructs })
  res <- doThis
  sets_ (\s -> s { structs = oldStrs })
  return res
  where
  newStructs = Map.fromList
               [ (sym, struct)
               | struct@(I.Struct sym _) <- I.public modStructs ++ I.private modStructs
               ]

data Value
  = Sint8  Int8
  | Sint16 Int16
  | Sint32 Int32
  | Sint64 Int64
  | Uint8  Word8
  | Uint16 Word16
  | Uint32 Word32
  | Uint64 Word64
  | Float  Float
  | Double Double
  | Char   Char
  | String String
  | Bool   Bool
  | Array  (Seq.Seq Value)
  | Struct (Map.Map I.Sym Value)
  | Ref    I.Sym
  deriving (Show, Eq, Ord)

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
or x        y        = error $ "invalid operands to `or`: " ++ show (x,y)

ordOp :: (forall a. Ord a => a -> a -> Bool) -> Value -> Value -> Value
ordOp op (Sint8  x) (Sint8  y) = Bool (op x y)
ordOp op (Sint16 x) (Sint16 y) = Bool (op x y)
ordOp op (Sint32 x) (Sint32 y) = Bool (op x y)
ordOp op (Sint64 x) (Sint64 y) = Bool (op x y)
ordOp op (Uint8  x) (Uint8  y) = Bool (op x y)
ordOp op (Uint16 x) (Uint16 y) = Bool (op x y)
ordOp op (Uint32 x) (Uint32 y) = Bool (op x y)
ordOp op (Uint64 x) (Uint64 y) = Bool (op x y)
ordOp op (Float  x) (Float  y) = Bool (op x y)
ordOp op (Double x) (Double y) = Bool (op x y)
ordOp op (Char   x) (Char   y) = Bool (op x y)
ordOp _ x y = error $ "invalid operands to `ordOp`: " ++ show (x,y)

gt  :: Value -> Value -> Value
gt  = ordOp (>)

gte :: Value -> Value -> Value
gte = ordOp (>=)

lt  :: Value -> Value -> Value
lt  = ordOp (<)

lte :: Value -> Value -> Value
lte = ordOp (<=)

numUnOp :: (forall a. Num a => a -> a) -> Value -> Value
numUnOp op (Sint8  x) = Sint8  (op x)
numUnOp op (Sint16 x) = Sint16 (op x)
numUnOp op (Sint32 x) = Sint32 (op x)
numUnOp op (Sint64 x) = Sint64 (op x)
numUnOp op (Uint8  x) = Uint8  (op x)
numUnOp op (Uint16 x) = Uint16 (op x)
numUnOp op (Uint32 x) = Uint32 (op x)
numUnOp op (Uint64 x) = Uint64 (op x)
numUnOp op (Float  x) = Float  (op x)
numUnOp op (Double x) = Double (op x)
numUnOp _ x = error $ "invalid operands to `numUnOp`: " ++ show x

negate :: Value -> Value
negate = numUnOp Prelude.negate

numBinOp :: (forall a. Num a => a -> a -> a) -> Value -> Value -> Value
numBinOp op (Sint8  x) (Sint8  y) = Sint8  (op x y)
numBinOp op (Sint16 x) (Sint16 y) = Sint16 (op x y)
numBinOp op (Sint32 x) (Sint32 y) = Sint32 (op x y)
numBinOp op (Sint64 x) (Sint64 y) = Sint64 (op x y)
numBinOp op (Uint8  x) (Uint8  y) = Uint8  (op x y)
numBinOp op (Uint16 x) (Uint16 y) = Uint16 (op x y)
numBinOp op (Uint32 x) (Uint32 y) = Uint32 (op x y)
numBinOp op (Uint64 x) (Uint64 y) = Uint64 (op x y)
numBinOp op (Float  x) (Float  y) = Float  (op x y)
numBinOp op (Double x) (Double y) = Double (op x y)
numBinOp _ x y = error $ "invalid operands to `numBinOp`: " ++ show (x,y)

add :: Value -> Value -> Value
add = numBinOp (+)

sub :: Value -> Value -> Value
sub = numBinOp (-)

mul :: Value -> Value -> Value
mul = numBinOp (*)

div :: Value -> Value -> Value
(Sint8  x) `div` (Sint8  y) = Sint8  (x `Prelude.div` y)
(Sint16 x) `div` (Sint16 y) = Sint16 (x `Prelude.div` y)
(Sint32 x) `div` (Sint32 y) = Sint32 (x `Prelude.div` y)
(Sint64 x) `div` (Sint64 y) = Sint64 (x `Prelude.div` y)
(Uint8  x) `div` (Uint8  y) = Uint8  (x `Prelude.div` y)
(Uint16 x) `div` (Uint16 y) = Uint16 (x `Prelude.div` y)
(Uint32 x) `div` (Uint32 y) = Uint32 (x `Prelude.div` y)
(Uint64 x) `div` (Uint64 y) = Uint64 (x `Prelude.div` y)
(Float  x) `div` (Float  y) = Float  (x / y)
(Double x) `div` (Double y) = Double (x / y)
x          `div` y          = error $ "invalid operands to `div`: " ++ show (x,y)

mod :: Value -> Value -> Value
(Sint8  x) `mod` (Sint8  y) = Sint8  (x `Prelude.mod` y)
(Sint16 x) `mod` (Sint16 y) = Sint16 (x `Prelude.mod` y)
(Sint32 x) `mod` (Sint32 y) = Sint32 (x `Prelude.mod` y)
(Sint64 x) `mod` (Sint64 y) = Sint64 (x `Prelude.mod` y)
(Uint8  x) `mod` (Uint8  y) = Uint8  (x `Prelude.mod` y)
(Uint16 x) `mod` (Uint16 y) = Uint16 (x `Prelude.mod` y)
(Uint32 x) `mod` (Uint32 y) = Uint32 (x `Prelude.mod` y)
(Uint64 x) `mod` (Uint64 y) = Uint64 (x `Prelude.mod` y)
-- (Float  x) `mod` (Float  y) = Float  (x `Prelude.mod` y)
-- (Double x) `mod` (Double y) = Double (x `Prelude.mod` y)
x          `mod` y          = error $ "invalid operands to `mod`: " ++ show (x,y)

readStore :: I.Sym -> Eval Value
readStore sym = do
  st <- fmap store get
  case Map.lookup sym st of
    Nothing -> raise $ "Unbound variable: `" ++ sym ++ "'!"
    Just v  -> return v

writeStore :: I.Sym -> Value -> Eval ()
writeStore sym val = sets_ (\s -> s { store = Map.insert sym val (store s) })

modifyStore :: I.Sym -> (Value -> Value) -> Eval ()
modifyStore sym f = sets_ (\s -> s { store = Map.update (Just . f) sym (store s) })

updateLoc :: SrcLoc -> Eval ()
updateLoc loc = sets_ (\ s -> s { loc = loc })

lookupStruct :: String -> Eval I.Struct
lookupStruct str = do
  structs <- fmap structs get
  case Map.lookup str structs of
    Nothing  -> raise $ "Couldn't find struct: " ++ str
    Just str -> return str

----------------------------------------------------------------------
-- | Main Evaluator
----------------------------------------------------------------------
evalBlock :: I.Block -> Eval ()
evalBlock = mapM_ evalStmt

evalRequires :: [I.Require] -> Eval Bool
evalRequires = fmap Prelude.and . mapM (evalCond . I.getRequire)

evalCond :: I.Cond -> Eval Bool
evalCond cond = case cond of
  I.CondBool expr -> do
    val <- evalExpr I.TyBool expr
    case val of
      Bool True  -> return True
      Bool False -> return False
      _          -> raise $ "evalCond: expected boolean, got: " ++ show val
  I.CondDeref ty expr var cond -> do
    evalStmt (I.Deref ty var expr)
    evalCond cond

evalStmt :: I.Stmt -> Eval ()
evalStmt stmt = case stmt of
  I.Comment (I.SourcePos loc)
    -> updateLoc loc
  I.Assert expr
    -> evalAssert expr
  I.CompilerAssert expr
    -> evalAssert expr
  I.IfTE cond true false
    -> do b <- evalExpr I.TyBool cond
          case b of
            Bool True  -> evalBlock true
            Bool False -> evalBlock false
            _          -> raise $ "evalStmt: IfTE: expected true or false, got: " ++ show b
  I.Deref _ty var expr
    -> do val <- evalDeref _ty expr
          case val of
            Ref ref -> do
              val <- readStore ref
              writeStore (varSym var) val
            _ -> writeStore (varSym var) val
  I.Assign _ty var expr
    -> do val <- evalExpr _ty expr
          writeStore (varSym var) val
  I.Local ty var init
    -> do val <- evalInit ty init
          writeStore (varSym var) val
  I.AllocRef _ty var ref
    -> writeStore (varSym var) (Ref $ nameSym ref)
  I.Loop _ var expr incr body
    -> do val <- evalExpr I.ixRep expr
          writeStore (varSym var) val
          let (checkDone, stepFn, doneExpr) = case incr of
                I.IncrTo expr -> ((>=), (`add` Sint32 1), expr) -- XXX: don't hard-code ixrep
                I.DecrTo expr -> ((<=), (`sub` Sint32 1), expr)
          let step = modifyStore (varSym var) stepFn
          let done = do curVal  <- readStore (varSym var)
                        doneVal <- evalExpr I.ixRep doneExpr
                        return (checkDone curVal doneVal)
          untilM done (evalBlock body >> step)
  I.Return (I.Typed  _ty expr)
    -> void $ evalExpr _ty expr
  I.Store _ty (I.ExpAddrOfGlobal dst) expr
    -> do val <- evalExpr _ty expr
          writeStore dst val
  I.Store _ty (I.ExpVar dst) expr
    -> do val <- evalExpr _ty expr
          Ref ref <- readStore (varSym dst)
          writeStore ref val
  I.RefCopy _ty (I.ExpAddrOfGlobal dst) expr
    -> do val <- evalExpr _ty expr
          writeStore dst val
  _ -> error $ show stmt

evalDeref :: I.Type -> I.Expr -> Eval Value
evalDeref _ty expr = case expr of
  I.ExpSym sym -> readRef sym
  I.ExpVar var -> readRef (varSym var)
  I.ExpAddrOfGlobal sym -> readStore sym
  I.ExpIndex tarr arr tidx idx
    -> do Array arr  <- evalDeref tarr arr
          Sint32 idx <- evalExpr tidx idx
          return (arr `Seq.index` fromIntegral idx)
  I.ExpLabel tstr str lab
    -> do Struct str <- evalDeref tstr str
          return (fromJust $ Map.lookup lab str)
  _ -> error $ show expr

readRef :: I.Sym -> Eval Value
readRef sym = do
  val <- readStore sym
  case val of
    Ref ref -> readStore ref
    _       -> raise $ "Expected Ref, got: " ++ show val

evalAssert :: I.Expr -> Eval ()
evalAssert asrt = do
  b <- evalExpr I.TyBool asrt
  case b of
    Bool True -> return ()
    _         -> raise $ "Assertion failed: " ++ show (b,asrt)

evalExpr :: I.Type -> I.Expr -> Eval Value
evalExpr ty expr = case expr of
  I.ExpSym sym -> readStore sym
  I.ExpVar var -> readStore (varSym var)
  I.ExpLit lit -> evalLit ty lit
  I.ExpOp op exprs
    -> do let opTy = case op of
                I.ExpEq t -> t
                I.ExpNeq t -> t
                I.ExpGt _ t -> t
                I.ExpLt _ t -> t
                I.ExpIsNan t -> t
                I.ExpIsInf t -> t
                _ -> ty
          vals <- mapM (evalExpr opTy) exprs
          evalOp op vals
  I.ExpSafeCast fromTy expr
    -> do val <- evalExpr fromTy expr
          return (cast ty fromTy val)
  I.ExpToIx expr max
    -> fmap (`mod` Sint32 (fromInteger max)) (evalExpr I.ixRep expr)
  _ -> error $ show expr

cast :: I.Type -> I.Type -> Value -> Value
cast toTy _fromTy val = mkVal toTy integer
  where
  integer = case val of
    Sint8  i -> toInteger i
    Sint16 i -> toInteger i
    Sint32 i -> toInteger i
    Sint64 i -> toInteger i
    Uint8  i -> toInteger i
    Uint16 i -> toInteger i
    Uint32 i -> toInteger i
    Uint64 i -> toInteger i
    _        -> error $ "Expected number, got: " ++ show val

evalLit :: I.Type -> I.Literal -> Eval Value
evalLit ty lit = case lit of
  I.LitInteger i -> return (mkVal ty i)
  I.LitFloat c   -> return (Float c)
  I.LitDouble c  -> return (Double c)
  I.LitChar c    -> return (Char c)
  I.LitBool b    -> return (Bool b)
  I.LitString s  -> return (String s)
  _              -> raise $ "evalLit: can't handle: " ++ show lit

mkVal :: I.Type -> Integer -> Value
mkVal ty = case ty of
  I.TyInt  I.Int8   -> Sint8  . fromInteger
  I.TyInt  I.Int16  -> Sint16 . fromInteger
  I.TyInt  I.Int32  -> Sint32 . fromInteger
  I.TyInt  I.Int64  -> Sint64 . fromInteger
  I.TyWord I.Word8  -> Uint8  . fromInteger
  I.TyWord I.Word16 -> Uint16 . fromInteger
  I.TyWord I.Word32 -> Uint32 . fromInteger
  I.TyWord I.Word64 -> Uint64 . fromInteger
  I.TyIndex _       -> Sint32 . fromInteger -- XXX: don't hard-code index rep
  I.TyFloat         -> Float  . fromInteger
  I.TyDouble        -> Double . fromInteger
  I.TyBool          -> Bool   . toEnum . fromInteger
  I.TyChar          -> Char   . toEnum . fromInteger
  _ -> error $ "mkVal: " ++ show ty

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
    _          -> raise $ "evalOp: ExpCond: expected true or false, got: " ++ show cond
evalOp op args = raise $ "evalOp: can't handle: " ++ show (op, args)

evalInit :: I.Type -> I.Init -> Eval Value
evalInit ty init = case init of
  I.InitZero
    -> case ty of
         I.TyArr _ _  -> evalInit ty (I.InitArray [] True)
         I.TyStruct _ -> evalInit ty (I.InitStruct [])
         _            -> return (mkVal ty 0)
  I.InitExpr ty expr
    -> evalExpr ty expr
  I.InitArray inits _
    -> case ty of
         I.TyArr len ty
           -> Array . Seq.fromList
              <$> mapM (evalInit ty) (take len $ inits ++ repeat I.InitZero)
         _ -> raise $ "evalInit: InitArray: unexpected type: " ++ show ty
  I.InitStruct inits
    -> case ty of
         I.TyStruct str -> do
           I.Struct _ fields <- lookupStruct str
           zstr <- foldM (\ str (I.Typed ty fld) -> do
                             val <- evalInit ty I.InitZero
                             return (Map.insert fld val str))
                         Map.empty fields
           str <- foldM (\ str (fld, init) -> do
                            val <- evalInit (lookupTyped fld fields) init
                            return (Map.insert fld val str))
                        zstr inits
           return $ Struct str
         _ -> raise $ "evalInit: InitStruct: unexpected type: " ++ show ty

lookupTyped :: (Show a, Eq a) => a -> [I.Typed a] -> I.Type
lookupTyped a [] = error $ "lookupTyped: couldn't find: " ++ show a
lookupTyped a (I.Typed t x : xs)
  | a == x    = t
  | otherwise = lookupTyped a xs

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
