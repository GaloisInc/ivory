{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImplicitParams #-}
module Ivory.QuickCheck.Testable where

import           Control.Applicative
import           Control.Monad
import           Data.IORef
import           Ivory.Compile.C.CmdlineFrontend
import           Ivory.Language
import           Ivory.Language.Proc
import qualified Ivory.Language.Syntax           as I
import           Ivory.QuickCheck

import qualified GHC.Exts                        as GHC

import qualified Test.QuickCheck.Arbitrary       as A
import qualified Test.QuickCheck.Gen             as G

import           Data.Int
import           Data.Word

[ivory|
struct foo
  { foo_a :: Stored IFloat
  ; foo_b :: Stored Uint8
  }
|]

-- Function we want to generate inputs for.
func :: Def ('[Uint8
              -- , Ref s (Array 3 (Stored Uint8))
              -- , Ref s (Struct "foo")
              ] :-> Uint8)
func = proc "func" $ \u ->
  requires (u >? 5) $
  body $ do
  -- arrayMap $ \ix -> do
  --   a <- deref (arr ! ix)
  --   b <- deref (str ~> foo_b)
  --   store (arr ! ix) (a + b + u)
  ret 1

cmodule = package "func" $ do
  defStruct (Proxy :: Proxy "foo")
  incl func

contract :: Def (args :-> res) -> Def (args :-> IBool)
contract (DefProc (I.Proc {..}))
  = DefProc (I.Proc { I.procSym = procSym ++ "__contract_check"
                    , I.procRetTy = I.TyBool
                    , I.procArgs = procArgs
                    , I.procBody = [ I.Call procRetTy Nothing (I.NameSym procSym)
                                     [ I.Typed t (I.ExpVar v) | I.Typed t v <- procArgs ]
                                   , I.Return (I.Typed I.TyBool (I.ExpLit (I.LitBool True)))
                                   ]
                    , I.procRequires = procRequires
                    , I.procEnsures = []
                    })

data HList (a :: [*]) where
  Nil   :: HList '[]
  (:::) :: a -> HList bs -> HList (a ': bs)
infixr :::

-- hfoldl :: 
-- hfoldl f x Nil = x
-- hfoldl f x (h ::: t) = hfoldl f (f x h) t

instance AllHave Show as => Show (HList as) where
  show Nil         = "()"
  show (x ::: Nil) = show x
  show (x ::: xs)  = show x ++ ", " ++ show xs

type family Map (f :: a -> b) (xs :: [a]) :: [b] where
  Map f '[]       = '[]
  Map f (x ': xs) = f x ': Map f xs

type family Constraints (cs :: [GHC.Constraint]) :: GHC.Constraint
type instance Constraints '[]       = ()
type instance Constraints (c ': cs) = (c, Constraints cs)

type AllHave (c :: k -> GHC.Constraint) (xs :: [k]) = Constraints (Map c xs)

type family Args p where
  Args (args :-> res) = args

type family MkFun args res where
  MkFun '[]       res = res
  MkFun (x ': xs) res = x -> MkFun xs res

class SampleArgs args where
  sampleArgs :: Int -> IO [HList args]

instance SampleArgs '[] where
  sampleArgs n = replicateM n (return Nil)

instance (IvoryVar a, A.Arbitrary a, SampleArgs as) => SampleArgs (a ': as) where
  sampleArgs n = do
    ass <- sampleArgs n
    as  <- runIO $ samples n (A.arbitrary :: G.Gen a)
    return $ zipWith (:::) as ass

class Testable proc eff impl | proc eff -> impl, impl -> eff where
  applyAux  :: Proxy proc -> impl -> HList (Args proc) -> impl

instance Testable ('[] :-> IBool) eff (Ivory eff IBool) where
  applyAux _ x Nil = x

-- instance (IvoryCall (args :-> IBool) eff impl)
--     => Testable ((a ': args) :-> IBool) eff (a -> impl) where
--   applyAux _ f (x ::: xs)
--     = applyAux rest (f x) xs
--     where
--     rest = Proxy :: Proxy (args :-> IBool)

sample = undefined
apply = undefined

  -- sample :: Def p -> Int -> IO [HList (Args p)]
  -- apply :: Def p -> HList (Args p) -> Ivory eff IBool

-- instance ( IvoryCall (args :-> IBool) eff (MkFun args (Ivory eff IBool))
--          -- , impl ~ MkFun args (Ivory eff IBool)
--          , SampleArgs args
--          ) => Testable (args :-> IBool) eff (Ivory eff IBool) where
--   sampleAux
  -- sample _ n   = sampleArgs n
  -- apply p args = happly (call p :: MkFun args (Ivory eff IBool)) args
    -- where
    -- happly :: MkFun args1 res1 -> HList args1 -> res1
    -- happly x Nil        = x
    -- happly f (x ::: xs) = happly (f x) xs

  -- apply p args = hfoldl ($) (call p :: impl) args

-- instance Testable ('[] :-> IBool) where
--   sample _ n = replicateM n (return Nil)
--   apply p Nil = call p

-- instance (IvoryVar a, A.Arbitrary a) => Testable ('[a] :-> IBool) where
--   sample _ n = runIO $ do
--     as <- samples n (A.arbitrary :: G.Gen a)
--     return $ map (::: Nil) as
--   apply p (a ::: Nil) = call p a

-- instance ( IvoryVar a, A.Arbitrary a
--          , IvoryVar b, A.Arbitrary b
--          ) => Testable ('[a,b] :-> IBool) where
--   sample _ n = runIO $ do
--     as <- samples n (A.arbitrary :: G.Gen a)
--     bs <- samples n (A.arbitrary :: G.Gen b)
--     return $ zipWith (\a b -> a ::: b ::: Nil) as bs
--   apply p (a ::: b ::: Nil) = call p a b

-- instance ( IvoryVar a, A.Arbitrary a
--          , IvoryVar b, A.Arbitrary b
--          , IvoryVar c, A.Arbitrary c
--          ) => Testable ('[a,b,c] :-> IBool) where
--   sample _ n = runIO $ do
--     as <- samples n (A.arbitrary :: G.Gen a)
--     bs <- samples n (A.arbitrary :: G.Gen b)
--     cs <- samples n (A.arbitrary :: G.Gen c)
--     return $ zipWith3 (\a b c -> a ::: b ::: c ::: Nil) as bs cs
--   apply p (a ::: b ::: c ::: Nil) = call p a b c

check :: forall proc eff impl. Testable proc eff impl
      => Int -> Module -> Def proc -> IO ()
check n m prop@(DefProc p) = do
  inputs <- sampleProc m p n
  let main = proc "main" $ body $ do
               -- forM_ inputs $ \i -> do
               --   assert =<< apply prop i
               retVoid
  let test = package (I.modName m ++ "__test") $ do
               depend m
               incl prop
               incl main
  runCompiler [m, test] [] initialOpts { outDir = Just "test" }

mkUnique :: (?counter :: IORef Integer) => IO Integer
mkUnique = do
  i <- readIORef ?counter
  writeIORef ?counter (i+1)
  return i

sampleProc :: Module -> I.Proc -> Int -> IO [[I.Block]]
sampleProc m@(I.Module {..}) (I.Proc {..}) n
  = do c <- newIORef 0
       let ?counter = c
       args <- forM procArgs $ \ (I.Typed t _) -> do
         sampleType m t n
       return args
       undefined

sampleType :: (?counter :: IORef Integer)
           => Module -> I.Type -> Int -> IO [(I.Var, I.Block)]
sampleType m t n = case t of
  I.TyInt sz
    -> mapM (mkLocal t) =<< sampleInt sz n
  I.TyWord sz
    -> mapM (mkLocal t) =<< sampleWord sz n
  I.TyIndex i
    -> mapM (mkLocal t) =<< sampleIndex i n
  I.TyBool
    -> mapM (mkLocal t) =<< sampleBool n
  I.TyChar
    -> mapM (mkLocal t) =<< sampleChar n
  I.TyFloat
    -> mapM (mkLocal t) =<< sampleFloat n
  I.TyDouble
    -> mapM (mkLocal t) =<< sampleDouble n
  I.TyRef t
    -> mapM (mkRef t) =<< sampleType m t n
  I.TyConstRef t
    -> mapM (mkRef t) =<< sampleType m t n
  -- I.TyPtr t -> mapM mkPtr (sampleType m t n)

mkLocal :: (?counter :: IORef Integer) => I.Type -> I.Init -> IO (I.Var, I.Block)
mkLocal ty init = do
  n <- mkUnique
  let v = mkVar n
  return (v, [I.Local ty v init])

mkRef :: (?counter :: IORef Integer) => I.Type -> (I.Var, I.Block)
      -> IO (I.Var, I.Block)
mkRef ty (v, init) = do
  n <- mkUnique
  let r = I.VarName ("ref" ++ show n)
  return (r, init ++ [ I.AllocRef (I.TyRef ty) r (I.NameVar v) ])

mkVar :: Integer -> I.Var
mkVar n = I.VarName ("var" ++ show n)

mk :: Int -> G.Gen a -> IO [a]
mk n g = G.generate (G.vectorOf n g)

sampleInt :: I.IntSize -> Int -> IO [I.Init]
sampleInt sz n = do
  xs <- gen
  return [ I.InitExpr (I.TyInt sz) (I.ExpLit (I.LitInteger x))
         | x <- xs
         ]
  where
  gen = case sz of
    I.Int8  -> fmap fromIntegral <$> mk n (A.arbitrary :: G.Gen Int8)
    I.Int16 -> fmap fromIntegral <$> mk n (A.arbitrary :: G.Gen Int16)
    I.Int32 -> fmap fromIntegral <$> mk n (A.arbitrary :: G.Gen Int32)
    I.Int64 -> fmap fromIntegral <$> mk n (A.arbitrary :: G.Gen Int64)

sampleWord :: I.WordSize -> Int -> IO [I.Init]
sampleWord sz n = do
  xs <- gen
  return [ I.InitExpr (I.TyWord sz) (I.ExpLit (I.LitInteger x))
         | x <- xs
         ]
  where
  gen = case sz of
    I.Word8  -> fmap fromIntegral <$> mk n (A.arbitrary :: G.Gen Word8)
    I.Word16 -> fmap fromIntegral <$> mk n (A.arbitrary :: G.Gen Word16)
    I.Word32 -> fmap fromIntegral <$> mk n (A.arbitrary :: G.Gen Word32)
    I.Word64 -> fmap fromIntegral <$> mk n (A.arbitrary :: G.Gen Word64)

sampleIndex :: Integer -> Int -> IO [I.Init]
sampleIndex ix n = do
  xs <- mk n (A.arbitrary :: G.Gen Word64)
  return [ I.InitExpr (I.TyIndex ix) (I.ExpLit (I.LitInteger (fromIntegral x `mod` ix)))
         | x <- xs
         ]

sampleBool :: Int -> IO [I.Init]
sampleBool n = do
  bs <- mk n A.arbitrary
  return [I.InitExpr I.TyBool (I.ExpLit (I.LitBool b)) | b <- bs]

sampleChar :: Int -> IO [I.Init]
sampleChar n = do
  cs <- mk n A.arbitrary
  return [I.InitExpr I.TyChar (I.ExpLit (I.LitChar c)) | c <- cs]

sampleFloat :: Int -> IO [I.Init]
sampleFloat n = do
  cs <- mk n A.arbitrary
  return [I.InitExpr I.TyFloat (I.ExpLit (I.LitFloat c)) | c <- cs]

sampleDouble :: Int -> IO [I.Init]
sampleDouble n = do
  cs <- mk n A.arbitrary
  return [I.InitExpr I.TyDouble (I.ExpLit (I.LitDouble c)) | c <- cs]

