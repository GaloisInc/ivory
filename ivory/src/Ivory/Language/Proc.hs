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

import Ivory.Language.IBool
import Ivory.Language.Init
import Ivory.Language.Monad
import Ivory.Language.Proxy
import Ivory.Language.Ref
import Ivory.Language.Type
import Ivory.Language.Effects
import Ivory.Language.Scope
import qualified Ivory.Language.Effects as E
import qualified Ivory.Language.Syntax as AST


-- Function Type ---------------------------------------------------------------

-- | The kind of procedures.
data Proc k = [k] :-> k

class ProcType (sig :: Proc *) where
  procType :: Proxy sig -> (AST.Type,[AST.Type])

instance IvoryType r => ProcType ('[] :-> r) where
  procType _ = (ivoryType (Proxy :: Proxy r),[])

instance (IvoryType a, ProcType (args :-> r))
      => ProcType ((a ': args) :-> r) where
  procType _ = (r, ivoryType (Proxy :: Proxy a) : args)
    where
    (r,args) = procType (Proxy :: Proxy (args :-> r))


-- Function Pointers -----------------------------------------------------------

-- | Procedure pointers, pointing to a procedure or NULL (Nothing).
newtype ProcPtr (s :: RefScope) (sig :: Proc *) =
  ProcPtr { getProcPtr :: Maybe AST.Name }

instance ProcType sig => IvoryType (ProcPtr s sig) where
  ivoryType _ = AST.TyProc r args
    where
    (r,args) = procType (Proxy :: Proxy sig)

instance ProcType sig => IvoryVar (ProcPtr s sig) where
  wrapVar        = ProcPtr . Just . AST.NameVar
  unwrapExpr ptr = case getProcPtr ptr of
    Nothing                -> AST.ExpLit AST.LitNull
    Just (AST.NameSym sym) -> AST.ExpSym sym
    Just (AST.NameVar var) -> AST.ExpVar var

instance ProcType sig => IvoryExpr (ProcPtr s sig) where
  wrapExpr exp = ProcPtr $ case exp of
    AST.ExpLit AST.LitNull -> Nothing
    AST.ExpSym sym         -> Just (AST.NameSym sym)
    AST.ExpVar var         -> Just (AST.NameVar var)
    _                      -> error "Impossible ProcType"

instance ProcType sig => IvoryEq      (ProcPtr s      sig)
instance ProcType sig => IvoryStore   (ProcPtr Global sig)
instance ProcType sig => IvoryInit    (ProcPtr Global sig)
instance ProcType sig => IvoryZeroVal (ProcPtr Global sig) where
  izeroval = ival nullProcPtr

nullProcPtr :: ProcType sig => ProcPtr s sig
nullProcPtr  = ProcPtr Nothing

procPtr :: ProcType sig => Def sig -> ProcPtr Global sig
procPtr  = ProcPtr . Just . defSymbol


-- Function Symbols ------------------------------------------------------------

-- | Procedure definitions.
data Def (proc :: Proc *)
  = DefProc AST.Proc
  | DefExtern AST.Extern
  | DefImport AST.Import
    deriving (Show, Eq, Ord)

defSymbol :: Def sig -> AST.Name
defSymbol def = case def of
  DefProc p   -> AST.NameSym (AST.procSym p)
  DefExtern e -> AST.NameSym (AST.externSym e)
  DefImport i -> AST.NameSym (AST.importSym i)

instance ProcType sig => IvoryType (Def sig) where
  ivoryType _ = AST.TyProc r args
    where
    (r,args) = procType (Proxy :: Proxy sig)


-- Procedure Definition --------------------------------------------------------

-- | Procedure definition.
proc :: forall sig impl. IvoryProcDef sig impl => AST.Sym -> impl -> Def sig
proc name impl = defproc
  where
  (r,args)   = procType (Proxy :: Proxy sig)
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

class ProcType sig => IvoryProcDef (sig :: Proc *) impl | impl -> sig where
  procDef :: Closure -> Proxy sig -> impl -> ([AST.Var], Definition)

instance IvoryType ret => IvoryProcDef ('[] :-> ret) (Body ret) where
  procDef env _ b = (getEnv env, Defined (snd (primRunIvory (runBody b))))

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


-- External Functions ----------------------------------------------------------

-- | External function reference.
externProc :: forall sig. ProcType sig => AST.Sym -> Def sig
externProc sym = DefExtern AST.Extern
  { AST.externSym     = sym
  , AST.externRetType = r
  , AST.externArgs    = args
  }
  where
  (r,args) = procType (Proxy :: Proxy sig)


-- Imported Functions ----------------------------------------------------------

-- | Import a function from a C header.
importProc :: forall sig. ProcType sig => AST.Sym -> String -> Def sig
importProc sym file = DefImport AST.Import
  { AST.importSym      = sym
  , AST.importFile     = file
  , AST.importRetTy    = retTy
  , AST.importArgs     = args
  , AST.importRequires = []
  , AST.importEnsures  = []
  }
  where
  (retTy, argTys) = procType (Proxy :: Proxy sig)
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
call :: forall sig eff impl. IvoryCall sig eff impl => Def sig -> impl
call def = callAux (defSymbol def) (Proxy :: Proxy sig) []

class IvoryCall (sig :: Proc *) (eff :: E.Effects) impl
    | sig eff -> impl, impl -> eff where
  callAux :: AST.Name -> Proxy sig -> [AST.Typed AST.Expr] -> impl

instance IvoryVar r => IvoryCall ('[] :-> r) eff (Ivory eff r) where
  callAux sym _ args = do
    r <- freshVar "r"
    emit (AST.Call (ivoryType (Proxy :: Proxy r)) (Just r) sym (reverse args))
    return (wrapVar r)

instance (IvoryVar a, IvoryVar r, IvoryCall (args :-> r) eff impl)
    => IvoryCall ((a ': args) :-> r) eff (a -> impl) where
  callAux sym _ args a = callAux sym rest args'
    where
    rest  = Proxy :: Proxy (args :-> r)
    args' = typedExpr a : args

-- | Indirect calls. Takes a default value and a procedure pointer. Returns the
-- default value if the pointer is null.
indirect :: forall sig eff r impl. IndirectIvoryCall sig eff r impl
         => r -> ProcPtr Global sig -> impl
indirect r ptr = indirectCallAux (getProcPtr ptr) (Proxy :: Proxy sig) r []

class IndirectIvoryCall (sig :: Proc *) (eff :: E.Effects) r impl
    | sig eff -> impl, impl -> eff, impl -> r where
  indirectCallAux
    :: Maybe AST.Name -> Proxy sig -> r -> [AST.Typed AST.Expr] -> impl

instance (GetAlloc eff ~ Scope s, IvoryVar r, IvoryStore r, IvoryZeroVal r)
      => IndirectIvoryCall ('[] :-> r) eff r (Ivory eff r) where
  indirectCallAux msym _ ret args
    | Nothing <- msym
    = return ret
    | Just sym <- msym
    = do
    r <- local izero
    ifte_ (nullProcPtr /=? (ProcPtr msym :: ProcPtr (s' :: RefScope) ('[] :-> r)))
      (do cRet <- freshVar "r"
          emit (AST.Call (ivoryType (Proxy :: Proxy r))
               (Just cRet) sym (reverse args))
          store r (wrapVar cRet)
      )
      (store r ret)
    deref r

instance (IvoryVar a, IvoryVar r, IndirectIvoryCall (args :-> r) eff r impl)
    => IndirectIvoryCall ((a ': args) :-> r) eff r (a -> impl) where
  indirectCallAux sym _ ret args a = indirectCallAux sym rest ret args'
    where
    rest  = Proxy :: Proxy (args :-> r)
    args' = typedExpr a : args

-- Call_ -----------------------------------------------------------------------

-- | Direct calls, ignoring the result.
call_ :: forall sig eff impl. IvoryCall_ sig eff impl => Def sig -> impl
call_ def = callAux_ (defSymbol def) (Proxy :: Proxy sig) []

class IvoryCall_ (sig :: Proc *) (eff :: E.Effects) impl
    | sig eff -> impl, impl -> eff
  where
  callAux_ :: AST.Name -> Proxy sig -> [AST.Typed AST.Expr] -> impl

instance IvoryType r => IvoryCall_ ('[] :-> r) eff (Ivory eff ()) where
  callAux_ sym _ args = do
    emit (AST.Call (ivoryType (Proxy :: Proxy r)) Nothing sym (reverse args))

instance (IvoryVar a, IvoryType r, IvoryCall_ (args :-> r) eff impl)
    => IvoryCall_ ((a ': args) :-> r) eff (a -> impl) where
  callAux_ sym _ args a = callAux_ sym rest args'
    where
    rest  = Proxy :: Proxy (args :-> r)
    args' = typedExpr a : args

-- | Indirect calls, ignoring the result. Elides a function call if the
-- procedure pointer is null.
indirect_ :: forall sig eff impl. IndirectIvoryCall_ sig eff impl
          => ProcPtr Global sig -> impl
indirect_ ptr = indirectCallAux_ (getProcPtr ptr) (Proxy :: Proxy sig) []

class IndirectIvoryCall_ (sig :: Proc *) (eff :: E.Effects) impl
    | sig eff -> impl, impl -> eff where
  indirectCallAux_
    :: Maybe AST.Name -> Proxy sig -> [AST.Typed AST.Expr] -> impl

instance IvoryType r => IndirectIvoryCall_ ('[] :-> r) eff (Ivory eff ()) where
  indirectCallAux_ msym _ args
    | Nothing <- msym
    = return ()
    | Just sym <- msym
    = do
    ifte_ (nullProcPtr /=? (ProcPtr msym :: ProcPtr (s' :: RefScope) ('[] :-> r)))
      (do cRet <- freshVar "r"
          emit (AST.Call (ivoryType (Proxy :: Proxy r))
               Nothing sym (reverse args))
      )
      (return ())

instance (IvoryVar a, IvoryVar r, IndirectIvoryCall_ (args :-> r) eff impl)
    => IndirectIvoryCall_ ((a ': args) :-> r) eff (a -> impl) where
  indirectCallAux_ sym _ args a = indirectCallAux_ sym rest args'
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
