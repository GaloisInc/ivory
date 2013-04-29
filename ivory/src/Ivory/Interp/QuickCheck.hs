{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.Interp.QuickCheck where

import Ivory.Interp.Error
import Ivory.Interp.Eval
import Ivory.Interp.Monad
import Ivory.Interp.Value
import Ivory.Language (IBool,Proxy(..))
import Ivory.Language.Monad (runIvory,CodeBlock(..))
import Ivory.Language.Proc (Body(..))
import Ivory.Language.Syntax
    (Typed(..),Type(..),IntSize(..),WordSize(..),Var(..),Stmt(Return)
    ,Expr(ExpVar))
import Ivory.Language.Type
import qualified Ivory.Language.Syntax as I

import Control.Monad (forM_,forM,replicateM)
import Data.IORef (newIORef,IORef)
import Data.Int (Int8,Int16,Int32,Int64)
import Data.Word (Word8,Word16,Word32,Word64)
import MonadLib (runM,ask)
import qualified Test.QuickCheck          as QC
import qualified Test.QuickCheck.Monadic  as QC
import qualified Test.QuickCheck.Property as QC


quickcheck :: IvoryProperty prop => prop -> Eval ()
quickcheck prop = do
  env <- Eval ask
  io (QC.quickCheck (QC.monadic (runner env) (evalProp prop)))

-- XXX not too sure about this, there could be trouble with refs if anything
-- global gets modified.  Maybe pack the whole env into an MVar?
runner :: IORef Env -> Eval QC.Property -> QC.Property
runner env (Eval body) = QC.morallyDubiousIOProperty $ do
  prop <- runM body env
  clearStack env
  return prop

-- | Name of the result to be bound in the property containing frame.
resultVar :: Var
resultVar  = VarInternal "result"

-- | Evaluate an Ivory property.
--
-- Push a new frame to hold the property.  Push a single instruction onto the
-- instruction stack that will return the value of the resultVar.  Proceed by
-- evaluating the property, which will generate all arguments and code, then
-- evaluate until the stack is empty.
evalProp :: IvoryProperty prop => prop -> QC.PropertyM Eval ()
evalProp prop = do
  -- property containing frame
  QC.run (pushFrame Nothing [Return (Typed TyBool (ExpVar resultVar))])
  -- generate vars and execute
  mb <- runProp emptyPropEnv prop
  QC.assert (maybe False isTrue mb)

-- | Property argument environment.
data PropEnv = PropEnv
  { envSupply :: [Var]
  , envArgs   :: [Typed (Var,Value)]
  }

-- | An empty property environment.
emptyPropEnv :: PropEnv
emptyPropEnv  = PropEnv
  { envSupply = [ VarName ("var" ++ show n) | n <- [0 :: Int ..] ]
  , envArgs   = []
  }

-- | Bind a new argument into the environment, and yield out its name.
propArg :: Type -> Value -> PropEnv -> (Var,PropEnv)
propArg ty val env = (var,env')
  where
  (var:rest) = envSupply env
  env'       = env
    { envSupply = rest
    , envArgs   = Typed ty (var,val) : envArgs env
    }


-- | Properties (functions) that can be evaluated.
class IvoryProperty prop where
  runProp :: PropEnv -> prop -> QC.PropertyM Eval (Maybe Value)

instance IvoryProperty (Body IBool) where
  runProp env body = do
    let (_,block) = runIvory (runBody body)
    QC.run $ do
      pushFrame (Just resultVar) (blockStmts block)
      forM_ (envArgs env) $ \ arg -> do
        let (_,val) = tValue arg
        bindArg (fst `fmap` arg) val
    mapM_ preCondition (blockRequires block)
    QC.run evalStack

instance (IvoryVar a, IvoryProperty prop) => IvoryProperty (a -> prop) where
  runProp env f = do
    let argTy = ivoryType (Proxy :: Proxy a)
    val <- typedValue argTy
    let (name,env') = propArg argTy val env
    QC.run (bindLocal name (Typed argTy val))
    runProp env' (f (wrapVar name))


-- Value Generation ------------------------------------------------------------

-- | Generate a value for the provided type.
typedValue :: Type -> QC.PropertyM Eval Value
typedValue ty = case ty of
  TyInt sz -> case sz of
    Int8  -> fromInt `fmap` QC.pick (QC.arbitraryBoundedRandom :: QC.Gen Int8)
    Int16 -> fromInt `fmap` QC.pick (QC.arbitraryBoundedRandom :: QC.Gen Int16)
    Int32 -> fromInt `fmap` QC.pick (QC.arbitraryBoundedRandom :: QC.Gen Int32)
    Int64 -> fromInt `fmap` QC.pick (QC.arbitraryBoundedRandom :: QC.Gen Int64)

  TyWord sz -> case sz of
    Word8  -> fromInt `fmap` QC.pick (QC.arbitraryBoundedRandom :: QC.Gen Word8)
    Word16 -> fromInt `fmap` QC.pick (QC.arbitraryBoundedRandom :: QC.Gen Word16)
    Word32 -> fromInt `fmap` QC.pick (QC.arbitraryBoundedRandom :: QC.Gen Word32)
    Word64 -> fromInt `fmap` QC.pick (QC.arbitraryBoundedRandom :: QC.Gen Word64)

  TyBool -> ValBool `fmap` QC.pick QC.arbitrary
  TyChar -> ValChar `fmap` QC.pick QC.arbitrary

  TyFloat  -> fromFloat  `fmap` QC.pick (QC.arbitrary :: QC.Gen Float)
  TyDouble -> fromDouble `fmap` QC.pick (QC.arbitrary :: QC.Gen Double)

  TyRef ety      -> ValRef `fmap` (allocRef =<< typedValue ety)
  TyConstRef ety -> ValRef `fmap` (allocRef =<< typedValue ety)

  TyPtr ety -> do
    isNull <- QC.pick QC.arbitrary
    if isNull
       then return (ValPtr Nothing)
       else (ValPtr . Just) `fmap` (allocRef =<< typedValue ety)

  TyArr len ety ->
    ValArray len `fmap` (replicateM len (allocRef =<< typedValue ety))

  TyStruct name -> do
    def <- QC.run (lookupStruct name)
    case def of

      I.Struct _ fs -> fmap ValStruct $ forM fs $ \ f -> do
        ref <- allocRef =<< typedValue (tType f)
        return (tValue f, ref)

      I.Abstract _ _ -> fail "Unable to generate values of abstract structs"

  TyProc{} ->
    QC.run (io (typeError "typedValue" "unable to generate function values"))

  TyVoid ->
    QC.run (io (typeError "typedValue" "unable to generate void values"))

  TyCArray{} ->
    QC.run (io (typeError "typedValue" "unable to generate CArray values"))




allocRef :: Value -> QC.PropertyM Eval (IORef Value)
allocRef val = QC.run (io (newIORef val))

preCondition :: I.Require -> QC.PropertyM Eval ()
preCondition r = loop (I.getRequire r)
  where
  loop cond = case cond of

    I.CondBool ex -> QC.pre . isTrue =<< QC.run (evalExpr I.TyBool ex)

    I.CondDeref ty ex n cond' -> do
      QC.run (evalStmt (I.Deref ty n ex))
      loop cond'
