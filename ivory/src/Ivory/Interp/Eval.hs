{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Interp.Eval where

import Ivory.Interp.Error (assumeFailed, assertFailed,typeError)
import Ivory.Interp.Monad
import Ivory.Interp.Value
import Ivory.Language.Monad
import Ivory.Language.Proxy
import Ivory.Language.Type
import qualified Ivory.Language.Syntax as AST

import Control.Applicative ((<$))
import Control.Monad (unless,void,zipWithM_,(<=<))
import Data.Char (chr)
import Data.IORef (newIORef)
import Data.Int (Int8,Int16,Int32,Int64)
import Data.Maybe (fromMaybe)
import Data.Word (Word8,Word16,Word32,Word64)
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq


-- Evaluation ------------------------------------------------------------------

-- | Produce a list of results.
scanlEval :: forall r a. FromValue r a
          => Int -> r
          -> (forall eff s. (eff `AllocsIn` s, eff `Returns` r) => r -> Ivory eff ())
          -> Eval [a]
scanlEval n0 a0 f = loop n0 Seq.empty =<< evalExpr ty (unwrapExpr a0)
  where
  interp = fromValue (Proxy :: Proxy r)
  ty     = ivoryType (Proxy :: Proxy r)

  setupFrame val = do
    let rname = AST.VarInternal "result"
    pushFrame Nothing [AST.Return (AST.Typed AST.TyBool (AST.ExpVar rname))]
    let arg   = AST.VarName "arg"
        stmts = blockStmts (snd (primRunIvory (f (wrapVar arg))))
    pushFrame (Just rname) stmts
    bindLocal arg (AST.Typed ty val)
    evalStack

  loop n rs val
    | n <= 0    = return (F.toList rs')
    | otherwise = do
      mb <- setupFrame val
      case mb of
        Just r' -> loop (n-1) rs' r'
        Nothing -> fail "Step did not return a value"
    where
    rs' = rs Seq.|> interp val


-- | Convenient evaluation of an @Ivory@ block.  This discards any require
-- statements that are present.
eval :: forall r a. (IvoryType r, FromValue r a)
     => (forall eff s. (eff `AllocsIn` s, eff `Returns` r) => Ivory eff ())
     -> Eval a
eval m = do
  pushFrame Nothing (blockStmts (snd (primRunIvory m)))
  mb <- evalStack
  case mb of
    Just val -> return (fromValue (Proxy :: Proxy r) val)
    Nothing  -> fail "eval didn't return a value"

-- | Evaluate until the stack is empty.
evalStack :: Eval (Maybe Value)
evalStack  = do
  mb <- getStmt
  case mb of
    Just stmt -> evalStmt stmt >> evalStack
    Nothing   -> getResult

-- | Evaluate a single @Ivory@ statement.
evalStmt :: AST.Stmt -> Eval ()
evalStmt stmt = case stmt of
  AST.IfTE cond tbranch fbranch -> do
    tst <- evalExpr AST.TyBool cond
    let branch | isTrue tst = tbranch
               | otherwise  = fbranch
    -- resolve the phi nodes when execution resumes
    pushCont branch (return ())

  AST.Assert e -> do
    val <- evalExpr AST.TyBool e
    unless (isTrue val) (io (assertFailed e))

  AST.Assume e -> do
    val <- evalExpr AST.TyBool e
    unless (isTrue val) (io (assumeFailed e))

  AST.Return te -> do
    val <- evalTypedExpr te
    setResult (Just val)                -- global result register
    bindResult (val <$ te) =<< popFrame -- caller result naming

  AST.ReturnVoid -> do
    setResult Nothing -- global result register
    void popFrame

  AST.Deref ty v ref -> do
    val <- evalExpr (AST.TyRef ty) ref
    r   <- io (readRef val)
    bindLocal v (AST.Typed ty r)

  AST.Store ty ref a -> do
    tgt <- evalExpr (AST.TyRef ty) ref
    val <- evalExpr ty a
    io (writeRef tgt val)

  AST.Assign ty var expr -> do
    val <- evalExpr ty expr
    bindLocal var (AST.Typed ty val)
    return ()

  AST.Call _ mbR fn args -> do
    def  <- evalCallSym fn
    vals <- mapM evalTypedExpr args
    pushFrame mbR (AST.procBody def)
    zipWithM_ bindArg (AST.procArgs def) vals

  AST.Local ty v i -> do
    ival <- evalInit ty i
    bindLocal v (AST.Typed ty ival)

  AST.RefCopy ty toRef fromRef -> do
    to   <- evalExpr (AST.TyRef ty) toRef
    from <- evalExpr (AST.TyRef ty) fromRef
    io (writeRef to from)

  AST.AllocRef ty l name -> case name of
    AST.NameVar r -> do
      val <- lookupLocal r
      ref <- io (newIORef val)
      bindLocal l (AST.Typed ty (ValRef ref))

    AST.NameSym _ -> do
      fail "unable to deal with memory areas"

  AST.Loop ix i incr body -> do
    let rep = AST.TyInt AST.Int32
    i' <- evalExpr rep i
    bindLocal ix (AST.Typed rep i')
    loopBody rep ix incr body

  AST.Forever _ -> do
    fail "unable to deal with forever loops"

  AST.Break ->
    popCont

evalCallSym :: AST.Name -> Eval AST.Proc
evalCallSym csym = case csym of
  AST.NameSym sym -> lookupProc sym
  AST.NameVar var -> do
    val <- lookupLocal var
    case val of
      ValSym sym -> lookupProc sym
      _          -> fail "evalCallSym: Runtime error: invalid call sym"


-- | Evaluate a loop, stashing the index increment and re-test inside of the
-- continuation restore action.
loopBody :: AST.Type -> AST.Var -> AST.LoopIncr -> AST.Block -> Eval ()
loopBody rep var incr body = loop
  where
  test   = testIncr rep var incr
  update = performIncr rep var incr
  loop   = do
    stop <- test
    unless stop (pushCont body loop >> update)

-- | Test the termination condition for a loop.
testIncr :: AST.Type -> AST.Var -> AST.LoopIncr -> Eval Bool
testIncr rep var incr = case incr of
  AST.IncrTo to -> cmp (AST.ExpGt True) to
  AST.DecrTo to -> cmp (AST.ExpLt True) to
  where
  cmp op to = fmap isTrue
            $ evalExpr AST.TyBool (AST.ExpOp (op rep) [AST.ExpVar var,to])

-- | Increment the loop variable.
performIncr :: AST.Type -> AST.Var -> AST.LoopIncr -> Eval ()
performIncr rep var incr = case incr of
  AST.IncrTo _ -> do
    -- increment the variable, and rebind it
    val <- evalExpr rep (AST.ExpVar var + 1)
    bindLocal var (AST.Typed rep val)

  AST.DecrTo _ -> do
    -- decrement the variable, and rebind it
    val <- evalExpr rep (AST.ExpVar var - 1)
    bindLocal var (AST.Typed rep val)

-- | Shortcut for @Typed Expr@ evaluation.
evalTypedExpr :: AST.Typed AST.Expr -> Eval Value
evalTypedExpr tv = evalExpr (AST.tType tv) (AST.tValue tv)

-- | Evaluate an expression down to a value.
evalExpr :: AST.Type -> AST.Expr -> Eval Value
evalExpr ty expr = case expr of
  AST.ExpSym sym ->
    return (ValSym sym)

  AST.ExpVar n ->
    lookupLocal n

  AST.ExpLit lit ->
    evalLit lit

  AST.ExpLabel _ e l -> do
    ref              <- evalExpr ty e
    ValStruct fields <- io (readRef ref)
    case lookup l fields of
      Just v  -> return (ValRef v)
      Nothing -> fail ("evalExp: Runtime error: no struct field named: " ++ l)

  AST.ExpIndex _ arr repTy ix -> do
    ival             <- evalExpr repTy ix
    ref              <- evalExpr ty arr
    ValArray _ elems <- io (readRef ref)
    return (ValRef $! elems !! extractInt ival)

  AST.ExpSafeCast fty e -> evalCast ty fty =<< evalExpr fty e

  AST.ExpToIx e maxSz -> do ValInt i <- evalExpr (AST.TyInt AST.Int32) e
                            return $ ValInt (i `rem` maxSz)

  AST.ExpOp op args -> evalOp ty op args

-- | Interpret a cast.
evalCast :: AST.Type -> AST.Type -> Value -> Eval Value
evalCast toTy fromTy val = case fromTy of
  -- identity cast
  _ | toTy == fromTy      -> return val

  -- from a boolean
  AST.TyBool -> case toTy of
    AST.TyChar   -> return (ValChar (chr (fromInteger bnum)))
    AST.TyInt  _ -> return (ValInt bnum)
    AST.TyWord _ -> return (ValInt bnum)
    _            -> io (typeError "evalCast" "invalid cast")
    where
    bnum = if isTrue val then 0 else 1

  AST.TyInt _    -> return (ValInt (extractInt val))
  AST.TyWord _   -> return (ValInt (extractInt val))

  _              -> io (typeError "evalCast" "invalid cast")

evalOp :: AST.Type -> AST.ExpOp -> [AST.Expr] -> Eval Value
evalOp ty op args = case (op,args) of

  -- eq instance
  (AST.ExpEq ety, [a,b]) -> interpEq ety (==) a b
  (AST.ExpNeq ety,[a,b]) -> interpEq ety (/=) a b

  -- conditional expressions
  (AST.ExpCond, [cond,t,f]) -> do
    cv <- evalExpr AST.TyBool cond
    tv <- evalExpr ty t
    fv <- evalExpr ty f
    if isTrue cv
       then return tv
       else return fv

  -- ord instance
  (AST.ExpGt orEq ety, [l,r]) -> interpOrd ety (if orEq then (>=) else (>)) l r
  (AST.ExpLt orEq ety, [l,r]) -> interpOrd ety (if orEq then (<=) else (<)) l r

  -- boolean operations
  (AST.ExpNot, [e]) -> (ValBool . isFalse) `fmap` evalExpr AST.TyBool e

  (AST.ExpAnd, [l,r]) -> do
    lv <- evalExpr AST.TyBool l
    rv <- evalExpr AST.TyBool r
    let lb = isTrue lv
        rb = isTrue rv
    lb `seq` rb `seq` return (ValBool (lb && rb))

  (AST.ExpOr, [l,r])  -> do
    lv <- evalExpr AST.TyBool l
    rv <- evalExpr AST.TyBool r
    let lb = isTrue lv
        rb = isTrue rv
    lb `seq` rb `seq` return (ValBool (lb || rb))

  -- num instance
  (AST.ExpMul,    [l,r]) -> interpNum2 ty (*) l r
  (AST.ExpAdd,    [l,r]) -> interpNum2 ty (+) l r
  (AST.ExpSub,    [l,r]) -> interpNum2 ty (-) l r
  (AST.ExpNegate, [a])   -> interpNum1 ty negate a
  (AST.ExpAbs,    [a])   -> interpNum1 ty abs a
  (AST.ExpSignum, [a])   -> interpNum1 ty signum a

  -- fractional/integral instances
  (AST.ExpDiv, [l,r]) -> case ty of
    AST.TyFloat  -> interpFloating2 ty (/) l r
    AST.TyDouble -> interpFloating2 ty (/) l r
    AST.TyInt _  -> interpIntegral2 ty div l r
    AST.TyWord _ -> interpIntegral2 ty div l r
    _            -> io (typeError "evalOp" "AST.ExpDiv")

  (AST.ExpMod, [l,r]) -> interpIntegral2 ty rem l r

  (AST.ExpRecip, [a]) -> interpFloating1 ty recip a

  -- floating instance
  (AST.ExpFExp,     [e])   -> interpFloating1 ty exp e
  (AST.ExpFSqrt,    [e])   -> interpFloating1 ty sqrt e
  (AST.ExpFLog,     [e])   -> interpFloating1 ty log e
  (AST.ExpFPow,     [l,r]) -> interpFloating2 ty (**) l r
  (AST.ExpFLogBase, [l,r]) -> interpFloating2 ty logBase l r
  (AST.ExpFSin,     [e])   -> interpFloating1 ty sin e
  (AST.ExpFTan,     [e])   -> interpFloating1 ty tan e
  (AST.ExpFCos,     [e])   -> interpFloating1 ty cos e
  (AST.ExpFAsin,    [e])   -> interpFloating1 ty asin e
  (AST.ExpFAtan,    [e])   -> interpFloating1 ty acos e
  (AST.ExpFAcos,    [e])   -> interpFloating1 ty atan e
  (AST.ExpFSinh,    [e])   -> interpFloating1 ty sinh e
  (AST.ExpFTanh,    [e])   -> interpFloating1 ty sinh e
  (AST.ExpFCosh,    [e])   -> interpFloating1 ty sinh e
  (AST.ExpFAsinh,   [e])   -> interpFloating1 ty asinh e
  (AST.ExpFAtanh,   [e])   -> interpFloating1 ty asinh e
  (AST.ExpFAcosh,   [e])   -> interpFloating1 ty asinh e

  -- floating operations
  (AST.ExpIsNan ety, [e]) -> do
    ev <- evalExpr ety e
    case ty of
      AST.TyFloat  -> return (ValBool (isNaN (extractFloat ev)))
      AST.TyDouble -> return (ValBool (isNaN (extractDouble ev)))
      _            -> io (typeError "evalOp" "ExpIsNan")

  (AST.ExpIsInf ety, [e]) -> do
    ev <- evalExpr ety e
    case ty of
      AST.TyFloat  -> return (ValBool (isInfinite (extractFloat ev)))
      AST.TyDouble -> return (ValBool (isInfinite (extractDouble ev)))
      _            -> io (typeError "evalOp" "ExpIsInf")

  (AST.ExpRoundF,[e]) -> interpRealFrac1 ty round   e
  (AST.ExpCeilF,[e])  -> interpRealFrac1 ty ceiling e
  (AST.ExpFloorF,[e]) -> interpRealFrac1 ty floor   e

  -- floating conversion
  (AST.ExpToFloat ety, [e]) -> do
    ev <- evalExpr ety e
    case ty of
      AST.TyFloat  -> return (ValFloat  (fromInteger (extractInt ev)))
      AST.TyDouble -> return (ValDouble (fromInteger (extractInt ev)))
      _            -> io (typeError "evalOp" "ExpToFloat")

  (AST.ExpFromFloat ety, [e]) -> do
    ev <- evalExpr ety e
    case ety of
      AST.TyFloat  -> return (ValInt (truncate (extractFloat ev)))
      AST.TyDouble -> return (ValInt (truncate (extractDouble ev)))
      _            -> io (typeError "evalOp" "ExpFromFloat")


  (_,_) -> io (typeError "evalOp" "unhandled operator")

-- | Cheat a little bit on interpreting equality.
interpEq :: AST.Type
         -> (Value -> Value -> Bool)
         -> (AST.Expr -> AST.Expr -> Eval Value)
interpEq ty op l r = do
  lv <- evalExpr ty l
  rv <- evalExpr ty r
  return (ValBool (op lv rv))

interpOrd :: AST.Type
          -> (forall a. Ord a => a -> a -> Bool)
          -> (AST.Expr -> AST.Expr -> Eval Value)
interpOrd ty op l r = do
  lv <- evalExpr ty l
  rv <- evalExpr ty r
  case ty of

    AST.TyBool -> return (ValBool (op (isTrue lv) (isTrue rv)))

    AST.TyInt sz -> return $ ValBool $ case sz of
      AST.Int8  -> op (extractInt lv) (extractInt rv :: Int8)
      AST.Int16 -> op (extractInt lv) (extractInt rv :: Int16)
      AST.Int32 -> op (extractInt lv) (extractInt rv :: Int32)
      AST.Int64 -> op (extractInt lv) (extractInt rv :: Int64)

    AST.TyWord sz -> return $ ValBool $ case sz of
      AST.Word8  -> op (extractInt lv) (extractInt rv :: Word8)
      AST.Word16 -> op (extractInt lv) (extractInt rv :: Word16)
      AST.Word32 -> op (extractInt lv) (extractInt rv :: Word32)
      AST.Word64 -> op (extractInt lv) (extractInt rv :: Word64)

    AST.TyFloat -> return (ValBool (op (extractFloat lv) (extractFloat rv)))

    AST.TyDouble -> return (ValBool (op (extractDouble lv) (extractDouble rv)))

    _ -> io (typeError "interpOrd" "")

interpNum2 :: AST.Type
           -> (forall a. Num a => a -> a -> a)
           -> (AST.Expr -> AST.Expr -> Eval Value)
interpNum2 ty op l r = do
  lv <- evalExpr ty l
  rv <- evalExpr ty r
  case ty of

    AST.TyInt sz -> case sz of
      AST.Int8  -> return (fromInt (op (extractInt lv) (extractInt rv :: Int8)))
      AST.Int16 -> return (fromInt (op (extractInt lv) (extractInt rv :: Int16)))
      AST.Int32 -> return (fromInt (op (extractInt lv) (extractInt rv :: Int32)))
      AST.Int64 -> return (fromInt (op (extractInt lv) (extractInt rv :: Int64)))

    AST.TyWord sz -> case sz of
      AST.Word8  -> return (fromInt (op (extractInt lv) (extractInt rv :: Word8)))
      AST.Word16 -> return (fromInt (op (extractInt lv) (extractInt rv :: Word16)))
      AST.Word32 -> return (fromInt (op (extractInt lv) (extractInt rv :: Word32)))
      AST.Word64 -> return (fromInt (op (extractInt lv) (extractInt rv :: Word64)))

    AST.TyFloat -> return (fromFloat (op (extractFloat lv) (extractFloat rv)))

    AST.TyDouble -> return (fromDouble (op (extractDouble lv) (extractDouble rv)))

    _ -> io (typeError "interpNum2" "")


interpNum1 :: AST.Type
           -> (forall a. Num a => a -> a)
           -> (AST.Expr -> Eval Value)
interpNum1 ty op a = do
  av <- evalExpr ty a
  case ty of

    AST.TyInt sz -> case sz of
      AST.Int8  -> return (fromInt (op (extractInt av :: Int8)))
      AST.Int16 -> return (fromInt (op (extractInt av :: Int16)))
      AST.Int32 -> return (fromInt (op (extractInt av :: Int32)))
      AST.Int64 -> return (fromInt (op (extractInt av :: Int64)))

    AST.TyWord sz -> case sz of
      AST.Word8  -> return (fromInt (op (extractInt av :: Word8)))
      AST.Word16 -> return (fromInt (op (extractInt av :: Word16)))
      AST.Word32 -> return (fromInt (op (extractInt av :: Word32)))
      AST.Word64 -> return (fromInt (op (extractInt av :: Word64)))

    AST.TyFloat -> return (fromFloat (op (extractFloat av)))

    AST.TyDouble -> return (fromDouble (op (extractDouble av)))

    _ -> io (typeError "interpNum1" "")

interpIntegral2 :: AST.Type
                -> (forall a. Integral a => a -> a -> a)
                -> (AST.Expr -> AST.Expr -> Eval Value)
interpIntegral2 ty op l r = do
  lv <- evalExpr ty l
  rv <- evalExpr ty r
  case ty of

    AST.TyInt sz -> case sz of
      AST.Int8  -> return (fromInt (op (extractInt lv) (extractInt rv :: Int8)))
      AST.Int16 -> return (fromInt (op (extractInt lv) (extractInt rv :: Int16)))
      AST.Int32 -> return (fromInt (op (extractInt lv) (extractInt rv :: Int32)))
      AST.Int64 -> return (fromInt (op (extractInt lv) (extractInt rv :: Int64)))

    AST.TyWord sz -> case sz of
      AST.Word8  -> return (fromInt (op (extractInt lv) (extractInt rv :: Word8)))
      AST.Word16 -> return (fromInt (op (extractInt lv) (extractInt rv :: Word16)))
      AST.Word32 -> return (fromInt (op (extractInt lv) (extractInt rv :: Word32)))
      AST.Word64 -> return (fromInt (op (extractInt lv) (extractInt rv :: Word64)))

    _ -> io (typeError "interpIntegral2" "")


interpFloating2 :: AST.Type
                -> (forall a. Floating a => a -> a -> a)
                -> (AST.Expr -> AST.Expr -> Eval Value)
interpFloating2 ty op l r = do
  lv <- evalExpr ty l
  rv <- evalExpr ty r
  case ty of
    AST.TyFloat  -> return (fromFloat  (op (extractFloat lv)  (extractFloat rv)))
    AST.TyDouble -> return (fromDouble (op (extractDouble lv) (extractDouble rv)))
    _            -> io (typeError "interpFloating2" "")

interpFloating1 :: AST.Type
                -> (forall a. Floating a => a -> a)
                -> (AST.Expr -> Eval Value)
interpFloating1 ty op a = do
  av <- evalExpr ty a
  case ty of
    AST.TyFloat  -> return (fromFloat  (op (extractFloat av)))
    AST.TyDouble -> return (fromDouble (op (extractDouble av)))
    _            -> io (typeError "interpFloating1" "")

interpRealFrac1 :: AST.Type
                -> (forall a. RealFrac a => a -> Integer)
                -> (AST.Expr -> Eval Value)
interpRealFrac1 ty op a = do
  av <- evalExpr ty a
  case ty of
    AST.TyFloat  -> return (fromFloat  (fromInteger (op (extractFloat av))))
    AST.TyDouble -> return (fromDouble (fromInteger (op (extractDouble av))))
    _            -> io (typeError "interpRealFrac1" "")

evalLit :: AST.Literal -> Eval Value
evalLit lit = case lit of
  AST.LitInteger n -> return (ValInt n)
  AST.LitFloat f   -> return (ValFloat f)
  AST.LitDouble d  -> return (ValDouble d)
  AST.LitChar c    -> return (ValChar c)
  AST.LitBool b    -> return (ValBool b)
  AST.LitNull      -> return  ValNull
  AST.LitString s  -> return (ValStr s)


evalInit :: AST.Type -> AST.Init -> Eval Value
evalInit ty i = case ty of

  AST.TyStruct n -> do
    s <- lookupStruct n
    initStruct s i

  AST.TyArr len ety -> initArray len ety i

  AST.TyRef ety      -> io . newRef =<< evalInit ety i
  AST.TyConstRef ety -> io . newRef =<< evalInit ety i

  AST.TyPtr ety -> case i of
    AST.InitExpr _ e -> io . newPtr . Just =<< evalExpr ety e
    AST.InitZero     -> io (newPtr Nothing)
    _                -> fail "Invalid pointer initializer"


  AST.TyInt{}  -> initSimple 0
  AST.TyWord{} -> initSimple 0
  AST.TyFloat  -> initSimple (AST.ExpLit (AST.LitFloat 0.0))
  AST.TyDouble -> initSimple (AST.ExpLit (AST.LitDouble 0.0))
  AST.TyBool   -> initSimple (AST.ExpLit (AST.LitBool False))
  AST.TyChar   -> initSimple (AST.ExpLit (AST.LitChar '\0'))

  AST.TyVoid     -> fail "Invalid void initializer"
  AST.TyProc{}   -> fail "Invalid proc initializer"
  AST.TyCArray{} -> fail "Invalid CArray initializer"

  where
  initSimple z = case i of
    AST.InitExpr _ e -> evalExpr ty e
    AST.InitZero     -> evalExpr ty z
    _                -> fail ("invalid initializer for " ++ show ty)


flattenStructInit :: AST.Init -> [(String,AST.Init)]
flattenStructInit i = case i of
  AST.InitStruct fs -> fs
  _                 -> []


initStruct :: AST.Struct -> AST.Init -> Eval Value
initStruct s i = case s of
  AST.Struct _ fs -> ValStruct `fmap` mapM initField fs
  AST.Abstract{}  -> fail "Unable to initialize an abstract struct"
  where
  is            = flattenStructInit i
  initField tyf = do
    let l = AST.tValue tyf
        e = fromMaybe AST.zeroInit (lookup l is)
    val <- evalInit (AST.tType tyf) e
    ref <- io (newIORef val)
    return (l,ref)


initArray :: Int -> AST.Type -> AST.Init -> Eval Value
initArray len ety i = do
  is <- case i of
    AST.InitZero     -> return (replicate len AST.InitZero)
    AST.InitArray is -> return is
    _                -> fail "invalid array initializer"
  let vs = take len (is ++ repeat AST.InitZero)
  ValArray len `fmap` mapM (io . newIORef <=< evalInit ety) vs
