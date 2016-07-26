{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

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
  , showErrors
  , existErrors
  , Results()
  , render
  ) where

import Prelude ()
import Prelude.Compat

import           Control.Monad (unless)
import qualified Data.Map                                as M
import           MonadLib
                     (WriterM(..),StateM(..),sets_,runId,runStateT,runWriterT
                     ,Id,StateT,WriterT)
import           Text.PrettyPrint

import           Ivory.Language.Syntax.Concrete.Location
import           Ivory.Language.Syntax.Concrete.Pretty
import qualified Ivory.Language.Array                    as I
import qualified Ivory.Language.Syntax.AST               as I
import qualified Ivory.Language.Syntax.Names             as I
import qualified Ivory.Language.Syntax.Type              as I

--------------------------------------------------------------------------------
-- Errors types

data Error = UnboundValue String
           | TypeError String I.Type I.Type
  deriving (Show, Eq)

data Warning = TypeWarning String I.Type I.Type
  deriving (Show, Eq)

data Results = Results
  { errors    :: [Located Error]
  , _warnings :: [Located Warning]
  } deriving (Show, Eq)

instance Monoid Results where
  mempty = Results [] []
  Results a0 b0 `mappend` Results a1 b1 = Results (a0 ++ a1) (b0 ++ b1)

-- | Are there any errors from typechecking?
existErrors :: Results -> Bool
existErrors = not . null . errors

showError :: Error -> Doc
showError err = case err of
  UnboundValue x
    -> text "Unbound value:" <+> quotes (text x)
  TypeError x actual expected
    -> typeMsg x actual expected

typeMsg :: String -> I.Type -> I.Type -> Doc
typeMsg x actual expected =
     quotes (text x) <+> text "has type:"
  $$ nest 4 (quotes (pretty actual))
  $$ text "but is used with type:"
  $$ nest 4 (quotes (pretty expected))

showWithLoc :: (a -> Doc) -> Located a -> Doc
showWithLoc sh (Located loc a) = pretty loc <> text ":" $$ nest 2 (sh a)

-- | Given a procedure name, show all the typechecking results for that procedure.
showErrors :: String -> Results -> Doc
showErrors procName res
  = mkOut procName "ERROR" (showWithLoc showError) (errors res)

mkOut :: String -> String -> (a -> Doc) -> [a] -> Doc
mkOut _   _    _  [] = empty
mkOut sym kind sh ls = nm $$ nest 4 (vcat (map go ls)) $$ empty
  where
  go x = text kind <> text ":" <+> sh x
  nm   = text "*** Procedure" <+> text sym

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
  put (Results [err `at` loc] [])

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

sanityCheck :: [I.Module] -> I.Module -> Results
sanityCheck deps this@(I.Module {..})
  = mconcat $ map (sanityCheckProc topLevel) $ getVisible modProcs
  where
  getVisible v = I.public v ++ I.private v

  topLevel :: M.Map String MaybeType
  topLevel = M.fromList
           $ concat [ procs m
                   ++ imports m
                   ++ externs m
                   ++ areas m
                   ++ importAreas m
                    | m <- this:deps
                    ]

  procs m       = [ (procSym, Defined $ I.TyProc procRetTy (map getType procArgs) )
                  | I.Proc {..} <- getVisible $ I.modProcs m ]
  imports m     = [ (importSym, Imported)
                  | I.Import {..} <- I.modImports m ]
  externs m     = [ (externSym, Defined externType)
                  | I.Extern {..} <- I.modExterns m ]
  areas m       = [ (areaSym, Defined areaType)
                  | I.Area {..} <- getVisible $ I.modAreas m ]
  importAreas m = [ (aiSym, Imported)
                  | I.AreaImport {..} <- I.modAreaImports m ]

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
    I.Local t v _
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
    I.TyArr n t
      -> text "Array" <+> pretty n <+> pretty t
    I.TyStruct s
      -> text "Struct" <+> pretty s
    I.TyCArray t
      -> text "CArray" <+> pretty t
    I.TyOpaque
      -> text "Opaque"

--------------------------------------------------------------------------------
-- Unused for now.

-- showWarning :: Warning -> Doc
-- showWarning w = case w of
--   TypeWarning x actual expected
--     -> typeMsg x actual expected

-- -- | Given a procedure name, show all the typechecking results for that procedure.
-- showWarnings :: String -> Results -> Doc
-- showWarnings procName res
--   = mkOut procName "WARNING" (showWithLoc showWarning) (warnings res)

-- putWarn :: Warning -> SCResults ()
-- putWarn warn = do
--   loc <- getStLoc
--   put (Results [] [warn `at` loc])

