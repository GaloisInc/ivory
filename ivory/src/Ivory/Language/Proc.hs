{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}

module Ivory.Language.Proc where

import Ivory.Language.Monad
import Ivory.Language.Proxy
import Ivory.Language.Type
import Ivory.Language.Effects
import qualified Ivory.Language.Effects as E
import qualified Ivory.Language.Syntax as AST


-- Function Type ---------------------------------------------------------------

-- | The kind of procedures.
data Proc k = [k] :-> k

-- | Typeclass for procedure types, parametrized over the C procedure's
-- signature, to produce a value representing their signature.
class ProcType (sig :: Proc *) where

  -- | Turn a type-level description of the signature into a (return
  -- type, [argument types]) value.
  procType :: Proxy sig -> (AST.Type,[AST.Type])

-- Base case: C procedure taking no arguments and returning an
-- 'IvoryType'.
instance IvoryType r => ProcType ('[] :-> r) where
  procType _ = (ivoryType (Proxy :: Proxy r),[])

-- Inductive case: Anything in 'ProcType' is still in 'ProcType' if it
-- has another 'IvoryType' argument prepended to its signature.
instance (IvoryType a, ProcType (args :-> r))
      => ProcType ((a ': args) :-> r) where
  procType _ = (r, ivoryType (Proxy :: Proxy a) : args)
    where
    (r,args) = procType (Proxy :: Proxy (args :-> r))


-- Function Pointers -----------------------------------------------------------

-- | Procedure pointers
newtype ProcPtr (sig :: Proc *) = ProcPtr { getProcPtr :: AST.Name }

instance ProcType proc => IvoryType (ProcPtr proc) where
  ivoryType _ = AST.TyProc r args
    where
    (r,args) = procType (Proxy :: Proxy proc)

instance ProcType proc => IvoryVar (ProcPtr proc) where
  wrapVar        = ProcPtr . AST.NameVar
  unwrapExpr ptr = case getProcPtr ptr of
    AST.NameSym sym -> AST.ExpSym sym
    AST.NameVar var -> AST.ExpVar var

procPtr :: ProcType sig => Def sig -> ProcPtr sig
procPtr  = ProcPtr . defSymbol


-- Function Symbols ------------------------------------------------------------

-- | Procedure definitions.
data Def (proc :: Proc *)
  = DefProc AST.Proc
  | DefImport AST.Import
    deriving (Show, Eq, Ord)

defSymbol :: Def proc -> AST.Name
defSymbol def = case def of
  DefProc p   -> AST.NameSym (AST.procSym p)
  DefImport i -> AST.NameSym (AST.importSym i)

instance ProcType proc => IvoryType (Def proc) where
  ivoryType _ = AST.TyProc r args
    where
    (r,args) = procType (Proxy :: Proxy proc)


-- Procedure Definition --------------------------------------------------------

-- | Procedure definition.
proc :: forall proc impl. IvoryProcDef proc impl => AST.Sym -> impl -> Def proc
proc name impl = defproc
  where
  (r,args)   = procType (Proxy :: Proxy proc)
  (vars,def) = procDef initialClosure Proxy impl

  defproc = case def of
        Defined block -> DefProc $
          AST.Proc { AST.procSym      = name
                   , AST.procRetTy    = r
                   , AST.procArgs     = zipWith AST.Typed args vars
                   , AST.procBody     = blockStmts block
                   , AST.procRequires = blockRequires block
                   , AST.procEnsures  = blockEnsures block
                   }
        Imported header reqs ens -> DefImport $
          AST.Import { AST.importSym      = name
                     , AST.importFile     = header
                     , AST.importRetTy    = r
                     , AST.importArgs     = zipWith AST.Typed args vars
                     , AST.importRequires = reqs
                     , AST.importEnsures  = ens
                     }

-- | Type inference can usually determine the argument types of an Ivory
-- procedure, but for void procedures there's often nothing to constrain
-- the return type. This function is a type-constrained version of
-- 'proc' that just forces the return type to be '()'.
voidProc :: IvoryProcDef (args :-> ()) impl =>
            AST.Sym -> impl -> Def (args :-> ())
voidProc = proc


newtype Body r = Body
  { runBody :: forall s . Ivory (E.ProcEffects s r) ()
  }

class WrapIvory m where
  type Return m
  wrap   :: (forall s . Ivory (E.ProcEffects s r) (Return m)) -> m r
  unwrap :: m r -> (forall s . Ivory (E.ProcEffects s r) (Return m))

instance WrapIvory Body where
  type Return Body = ()
  wrap   = Body
  unwrap = runBody

body :: IvoryType r
     => (forall s . Ivory (E.ProcEffects s r) ())
     -> Body r
body m = Body m


data Definition = Defined CodeBlock
                | Imported FilePath [AST.Require] [AST.Ensure]

-- | Typeclass for an Ivory procedure definition to produce ;
-- the type is parametrized over:
--
--   * The procedure type 'proc', encoding the C procedure's signature
--   via the 'Proc' kind,
--   * The implementation type 'impl' - either 'Body' for the return
--   value, or else a Haskell function type whose arguments correspond
--   to the C arguments and whose return type is @Body r@ on the return
--   type @r@.
class ProcType proc => IvoryProcDef (proc :: Proc *) impl | impl -> proc where
  procDef :: Closure -> Proxy proc -> impl -> ([AST.Var], Definition)

-- Base case: No arguments in C signature
instance IvoryType ret => IvoryProcDef ('[] :-> ret) (Body ret) where
  procDef env _ b = (getEnv env, Defined (snd (primRunIvory (runBody b))))

-- Inductive case: Remove first argument from C signature, and
-- parametrize 'impl' over another argument of the same type.
instance (IvoryVar a, IvoryProcDef (args :-> ret) k)
      => IvoryProcDef ((a ': args) :-> ret) (a -> k) where
  procDef env _ k = procDef env' (Proxy :: Proxy (args :-> ret)) (k arg)
    where
    (var,env') = genVar env
    arg        = wrapVar var

-- | A variable name supply, and the typed values that have been generated.
data Closure = Closure
  { closSupply :: [AST.Var]
  , closEnv    :: [AST.Var]
  }

-- | Initial closure, with no environment and a large supply of names.
initialClosure :: Closure
initialClosure  = Closure
  { closSupply = [ AST.VarName ("var" ++ show (n :: Int)) | n <- [0 ..] ]
  , closEnv    = []
  }

-- | Given a type and a closure, generate a typed variable, and a new closure
-- with that typed variable in it's environment.
genVar :: Closure -> (AST.Var, Closure)
genVar clos = (var, clos')
  where
  var   = head (closSupply clos)
  clos' = Closure
    { closSupply = tail (closSupply clos)
    , closEnv    = var : closEnv clos
    }

-- | Retrieve the environment from a closure.
getEnv :: Closure -> [AST.Var]
getEnv  = reverse . closEnv


-- Imported Functions ----------------------------------------------------------

-- | Import a function from a C header.
importProc :: forall proc. ProcType proc => AST.Sym -> String -> Def proc
importProc sym file = DefImport AST.Import
  { AST.importSym      = sym
  , AST.importFile     = file
  , AST.importRetTy    = retTy
  , AST.importArgs     = args
  , AST.importRequires = []
  , AST.importEnsures  = []
  }
  where
  (retTy, argTys) = procType (Proxy :: Proxy proc)
  args = zipWith AST.Typed argTys (closSupply initialClosure)

newtype ImportFrom r = ImportFrom
  { runImportFrom :: forall s . Ivory (E.ProcEffects s r) FilePath
  }

instance WrapIvory ImportFrom where
  type Return ImportFrom = FilePath
  wrap   = ImportFrom
  unwrap = runImportFrom

importFrom :: String -> ImportFrom a
importFrom h = ImportFrom (return h)

instance IvoryType ret => IvoryProcDef ('[] :-> ret) (ImportFrom ret) where
  procDef env _ b = (getEnv env, Imported header reqs ens)
    where
    (header, block) = primRunIvory (runImportFrom b)
    reqs            = blockRequires block
    ens             = blockEnsures block

-- Call ------------------------------------------------------------------------

-- | Direct calls.
call :: forall proc eff impl. IvoryCall proc eff impl => Def proc -> impl
call def = callAux (defSymbol def) (Proxy :: Proxy proc) []

-- | Indirect calls.
indirect :: forall proc eff impl. IvoryCall proc eff impl
         => ProcPtr proc -> impl
indirect ptr = callAux (getProcPtr ptr) (Proxy :: Proxy proc) []

-- | Typeclass for something callable in Ivory (and returning a
-- result).  Parameter 'proc' is the procedure type (encoding the
-- arguments and return of the C procedure via the 'Proc' kind, as in
-- 'IvoryProcDef'), parameter 'eff' is the effect context (which
-- remains unchanged through the calls here), and parameter 'impl', as
-- in 'IvoryProcDef', is the implementation type.
class IvoryCall (proc :: Proc *) (eff :: E.Effects) impl
    | proc eff -> impl, impl -> eff where

  -- | Recursive helper call.  'proc' encodes a C procedure type, and
  -- this call has two main parts:
  -- 
  --   * If 'proc' contains arguments, then 'impl' must be a function
  --   type causing this whole call to expect an Ivory value that was
  --   passed in to apply to the C procedure.  In this case, 'proc' is
  --   reduced by removing the first C argument from the type itself,
  --   and the argument to 'impl' is accumulated onto the list of
  --   typed expressions.
  --   * If 'proc' contains no arguments, then this returns the Ivory
  --   effect which calls the function with all the arguments in the
  --   list applied to it, and captures and returns the result.
  callAux :: AST.Name -> Proxy proc -> [AST.Typed AST.Expr] -> impl

instance IvoryVar r => IvoryCall ('[] :-> r) eff (Ivory eff r) where
  -- Base case ('proc' takes no arguments, 'impl' is just an Ivory
  -- effect):
  callAux sym _ args = do
    r <- freshVar "r"
    emit (AST.Call (ivoryType (Proxy :: Proxy r)) (Just r) sym (reverse args))
    return (wrapVar r)

instance (IvoryVar a, IvoryVar r, IvoryCall (args :-> r) eff impl)
    => IvoryCall ((a ': args) :-> r) eff (a -> impl) where
  -- Inductive case: note that 'proc' reduces from ((a ': args) :-> r)
  -- down to (args :-> r) in the proxy, and that 'impl' is (a -> impl)
  -- and we put that 'a' onto the list of arguments.  
  callAux sym _ args a = callAux sym rest args'
    where
    rest  = Proxy :: Proxy (args :-> r)
    args' = typedExpr a : args


-- Call_ -----------------------------------------------------------------------

-- | Direct calls, ignoring the result.
call_ :: forall proc eff impl. IvoryCall_ proc eff impl => Def proc -> impl
call_ def = callAux_ (defSymbol def) (Proxy :: Proxy proc) []

-- | Indirect calls, ignoring the result.
indirect_ :: forall proc eff impl. IvoryCall_ proc eff impl
          => ProcPtr proc -> impl
indirect_ ptr = callAux_ (getProcPtr ptr) (Proxy :: Proxy proc) []

-- | Typeclass for something callable in Ivory without a return value
-- needed.  This is otherwise identical to 'IvoryCall'.
class IvoryCall_ (proc :: Proc *) (eff :: E.Effects) impl
    | proc eff -> impl, impl -> eff
  where
  callAux_ :: AST.Name -> Proxy proc -> [AST.Typed AST.Expr] -> impl

instance IvoryType r => IvoryCall_ ('[] :-> r) eff (Ivory eff ()) where
  callAux_ sym _ args = do
    emit (AST.Call (ivoryType (Proxy :: Proxy r)) Nothing sym (reverse args))

instance (IvoryVar a, IvoryType r, IvoryCall_ (args :-> r) eff impl)
    => IvoryCall_ ((a ': args) :-> r) eff (a -> impl) where
  callAux_ sym _ args a = callAux_ sym rest args'
    where
    rest  = Proxy :: Proxy (args :-> r)
    args' = typedExpr a : args

-- Return ----------------------------------------------------------------------
-- | Primitive return from function.
ret :: (GetReturn eff ~ Returns r, IvoryVar r) => r -> Ivory eff ()
ret r = emit (AST.Return (typedExpr r))

-- | Primitive void return from function.
retVoid :: (GetReturn eff ~ Returns ()) => Ivory eff ()
retVoid  = emit AST.ReturnVoid
