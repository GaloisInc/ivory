{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

--
-- Sanity check to ensure all functions and memory areas are used
-- with the correct type.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Opts.SanityCheck
  ( sanityCheck
  , showSanityChkModule
  , existErrors
  , dupDefs
  , showDupDefs
  ) where

import           Prelude                                 ()
import           Prelude.Compat

import           System.IO                               (hPutStrLn, stderr)

import           Control.Monad                           (unless)
import qualified Data.List                               as L
import qualified Data.Map                                as M
import           MonadLib                                (Id, StateM (..),
                                                          StateT, WriterM (..),
                                                          WriterT, runId,
                                                          runStateT, runWriterT,
                                                          sets_)
import           Text.PrettyPrint

import qualified Ivory.Language.Array                    as I
import qualified Ivory.Language.Syntax.AST               as I
import           Ivory.Language.Syntax.Concrete.Location
import           Ivory.Language.Syntax.Concrete.Pretty
import qualified Ivory.Language.Syntax.Names             as I
import qualified Ivory.Language.Syntax.Type              as I
import           Ivory.Opts.Utils

--------------------------------------------------------------------------------
-- Errors types

data Error = UnboundValue String
           | TypeError String I.Type I.Type
  deriving (Show, Eq)

type Result = Located Error

type Results = [Located Error]

-- | Are there any errors from typechecking?
existErrors :: ModResult Result -> Bool
existErrors (ModResult _ ls) = or (map go ls)
  where
  go (SymResult _ res) = not (null res)

showError :: Error -> Doc
showError err = case err of
  UnboundValue x
    -> text "Unbound value:" <+> quotes (text x)
  TypeError x actual expected
    ->   quotes (text x) <+> text "has type:"
      $$ nest 4 (quotes (pretty actual))
      $$ text "but is used with type:"
      $$ nest 4 (quotes (pretty expected))

showSanityChkModule :: ModResult Result -> IO ()
showSanityChkModule res = showModErrs go res
  where
  go :: Result -> Doc
  go = showWithLoc showError

--------------------------------------------------------------------------------
-- Writer Monad

-- For imported things, we won't require that the string maps to a valid
-- type. For example, we might import `printf` multiple times at multiple
-- types. We'll only check that the key exists (that the symbol isn't
-- unbound). So imported symbols map to `Nothing`.
data MaybeType = Imported | Defined I.Type
  deriving (Show, Eq)

data St = St { loc :: SrcLoc, env :: M.Map String MaybeType }

newtype SCResults a = SCResults { unTC :: WriterT Results (StateT St Id) a }
  deriving (Functor, Applicative, Monad)

instance WriterM SCResults Results where
  put e = SCResults (put e)

instance StateM SCResults St where
  get = SCResults get
  set = SCResults . set

getStLoc :: SCResults SrcLoc
getStLoc = fmap loc get

setStLoc :: SrcLoc -> SCResults ()
setStLoc l = sets_ (\s -> s { loc = l })

localEnv :: SCResults a -> SCResults a
localEnv doThis = do
  env <- fmap env get
  res <- doThis
  sets_ (\s -> s { env = env })
  return res

checkScope :: String -> SCResults ()
checkScope name =
  lookupType name >>= \case
    Nothing -> putError (UnboundValue name)
    Just _  -> return ()

lookupType :: String -> SCResults (Maybe MaybeType)
lookupType name = do
  env <- fmap env get
  return $ M.lookup name env

hasType :: String -> I.Type -> SCResults ()
hasType name ty = sets_ (\s -> s { env = M.insert name (Defined ty) (env s) })

putError :: Error -> SCResults ()
putError err = do
  loc <- getStLoc
  put [err `at` loc]

runSCResults :: SCResults a -> (a, Results)
runSCResults tc = fst $ runId $ runStateT (St NoLoc M.empty) $ runWriterT (unTC tc)

--------------------------------------------------------------------------------

varString :: I.Var -> String
varString v = case v of
  I.VarName s -> s
  I.VarInternal s -> s
  I.VarLitName s -> s

nameString :: I.Name -> String
nameString n = case n of
  I.NameSym s -> s
  I.NameVar v -> varString v

getType :: I.Typed a -> I.Type
getType (I.Typed t _) = t

sanityCheck :: [I.Module] -> [ModResult Result]
sanityCheck ms = map goMod ms
  where
  goMod m =
    ModResult (I.modName m)
              (concatMap goProc (getVisible I.modProcs m))

  goProc p =
    let scp = sanityCheckProc topLevel p in
    if null scp then [] else [SymResult (I.procSym p) scp]

  topLevel :: M.Map String MaybeType
  topLevel = M.fromList $
             concat [ procs m
                   ++ imports m
                   ++ externs m
                   ++ areas m
                   ++ importAreas m
                    | m <- ms
                    ]

showDupDefs :: [String] -> IO ()
showDupDefs dups =
  if null dups then return ()
    else hPutStrLn stderr $ render (vcat (map docDups dups) $$ empty)
  where
  docDups x = text "*** WARNING"
           <> colon
          <+> quotes (text x)
          <+> text "has multiple definitions."

-- Are there any duplicated definitions?
dupDefs :: [I.Module] -> [String]
dupDefs ms = map fst dups
  where
  dups :: [(String, MaybeType)]
  -- XXX doesn't guarantee that there are multiply defined defs, but at
  -- least sanity checks it.
  dups = defs L.\\ (L.nubBy (\a b -> (fst a == fst b) && (snd a /= snd b))) defs
    where
    defs = concat [ procs m ++ areas m ++ structs m | m <- ms ]

getVisible :: (t -> I.Visible a) -> t -> [a]
getVisible acc m =
  let ps = acc m in
  I.public ps ++ I.private ps

procs, areas, imports, externs, importAreas, structs :: I.Module -> [(I.Sym, MaybeType)]
procs m       = [ (procSym, Defined $ I.TyProc procRetTy (map getType procArgs) )
                | I.Proc {..} <- getVisible I.modProcs m ]
areas m       = [ (areaSym, Defined areaType)
                | I.Area {..} <- getVisible I.modAreas m ]
imports m     = [ (importSym, Imported)
                | I.Import {..} <- I.modImports m ]
externs m     = [ (externSym, Defined externType)
                | I.Extern {..} <- I.modExterns m ]
importAreas m = [ (aiSym, Imported)
                | I.AreaImport {..} <- I.modAreaImports m ]
-- Not an imported type, but we don't need hte type here.
structs m     = [ (nm, Imported)
                | (I.Struct nm _) <- getVisible I.modStructs m ]

-- | Sanity Check a procedure. Check for unbound and ill-typed values
sanityCheckProc :: M.Map String MaybeType -> I.Proc -> Results
sanityCheckProc env (I.Proc {..}) = snd $ runSCResults $ do
  sets_ (\s -> s { env = env })
  mapM_ (\ (I.Typed t v) -> varString v `hasType` t) procArgs
  check procBody

check :: [I.Stmt] -> SCResults ()
check = mapM_ go
  where
  go stmt = case stmt of
    I.Deref t v e
      -> checkExpr e >> varString v `hasType` t
    I.Store _ e1 e2
      -> checkExpr e1 >> checkExpr e2
    I.Assign t v e
      -> checkExpr e >> varString v `hasType` t
    I.Call t mv (nameString -> f) args
      -> do mapM_ (\ (I.Typed _ e) -> checkExpr e) args
            mt <- lookupType f
            case mt of
              Nothing
                -> putError (UnboundValue f)
              Just mty
                -> case mty of
                     Imported   -> return ()
                     Defined ty -> checkCall f ty args t
            case mv of
              Nothing -> return ()
              Just v  -> varString v `hasType` t
    I.Local t v _init
      -> varString v `hasType` t
    I.RefCopy _ e1 e2
      -> checkExpr e1 >> checkExpr e2
    I.RefZero _ e1
      -> checkExpr e1
    I.AllocRef t v _
      -> varString v `hasType` t
    I.Loop _ v _ _ stmts
      -> localEnv $ do varString v `hasType` I.ixRep
                       check stmts
    I.Forever stmts
      -> localEnv $ check stmts
    I.IfTE b t f
      -> do checkExpr b
            localEnv $ check t
            localEnv $ check f
    I.Comment (I.SourcePos loc)
      -> setStLoc loc
    _ -> return ()

checkExpr :: I.Expr -> SCResults ()
checkExpr expr = case expr of
  I.ExpSym v           -> checkScope v
  I.ExpExtern v        -> checkScope (I.externSym v)
  I.ExpAddrOfGlobal v  -> checkScope v
  I.ExpVar v           -> checkScope (varString v)
  I.ExpLabel _ e _     -> checkExpr e
  I.ExpIndex _ e1 _ e2 -> checkExpr e1 >> checkExpr e2
  I.ExpToIx e _        -> checkExpr e
  I.ExpSafeCast _ e    -> checkExpr e
  I.ExpOp _ exprs      -> mapM_ checkExpr exprs
  _                    -> return ()

checkCall :: String -> I.Type -> [I.Typed I.Expr] -> I.Type -> SCResults ()
checkCall f ty args retTy = case ty of
  I.TyProc r as
    -> unless (all eq $ zip (r:as) (retTy : argTys))
         (putError $ TypeError f (I.TyProc r as) (I.TyProc retTy argTys))
  _ -> putError $ TypeError f ty (I.TyProc retTy argTys)
  where
  argTys   = [t | I.Typed t _ <- args]
  eq (x,y) = x == y

instance Pretty I.Type where
  pretty ty = case ty of
    I.TyVoid
      -> text "()"
    I.TyInt I.Int8
      -> text "Sint8"
    I.TyInt I.Int16
      -> text "Sint16"
    I.TyInt I.Int32
      -> text "Sint32"
    I.TyInt I.Int64
      -> text "Sint64"
    I.TyWord I.Word8
      -> text "Uint8"
    I.TyWord I.Word16
      -> text "Uint16"
    I.TyWord I.Word32
      -> text "Uint32"
    I.TyWord I.Word64
      -> text "Uint64"
    I.TyIndex i
      -> text "Ix" <+> text (show i)
    I.TyBool
      -> text "Bool"
    I.TyChar
      -> text "Char"
    I.TyFloat
      -> text "Float"
    I.TyDouble
      -> text "Double"
    I.TyProc ret args
      -> pretty args <+> text ":->" <+> pretty ret
    I.TyRef ref
      -> text "Ref" <+> pretty ref
    I.TyConstRef ref
      -> text "ConstRef" <+> pretty ref
    I.TyPtr ptr
      -> text "Ptr" <+> pretty ptr
    I.TyConstPtr ptr
      -> text "ConstPtr" <+> pretty ptr
    I.TyArr n t
      -> text "Array" <+> pretty n <+> pretty t
    I.TyStruct s
      -> text "Struct" <+> pretty s
    I.TyCArray t
      -> text "CArray" <+> pretty t
    I.TyOpaque
      -> text "Opaque"
