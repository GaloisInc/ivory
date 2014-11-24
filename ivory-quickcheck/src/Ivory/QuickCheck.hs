{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImplicitParams #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Check properties of Ivory programs using random inputs.
--
-- Example usage:
-- 
-- > [ivory|
-- > struct foo
-- >   { foo_a :: Stored IFloat
-- >   ; foo_b :: Stored Uint8
-- >   }
-- > |]
-- >
-- > -- Function we want to generate inputs for.
-- > func :: Def ('[Uint8
-- >               , Ref s (Array 3 (Stored Uint8))
-- >               , Ref s (Struct "foo")
-- >               ] :-> ())
-- > func = proc "func" $ \u arr str -> body $
-- >   ensures (const $ checkStored (arr ! 0) (\r -> r >? u)) $
-- >   arrayMap $ \ix -> do
-- >     a <- deref (arr ! ix)
-- >     b <- deref (str ~> foo_b)
-- >     store (arr ! ix) (a + b + u)
-- >
-- > -- Module containing our function
-- > cmodule :: Module
-- > cmodule = package "module" $ do
-- >   defStruct (Proxy :: Proxy "foo")
-- >   incl func
-- >
-- > -- Running @mkTest@ will produce a C program in @<pwd>/test@ that will check
-- > -- @func@'s contract on 10 random inputs.
-- > mkTest :: IO ()
-- > mkTest = check 10 cmodule (contract func)

module Ivory.QuickCheck (check, contract) where

import           Control.Applicative
import           Control.Monad
import           Data.IORef
import           Data.List
import           Ivory.Compile.C.CmdlineFrontend
import           Ivory.Language
import           Ivory.Language.Proc
import qualified Ivory.Language.Syntax           as I


import qualified Test.QuickCheck.Arbitrary       as A
import qualified Test.QuickCheck.Gen             as G

import           Data.Int
import           Data.Word

-- | Generate a random C program to check that the property holds. The
-- generated program will be placed in the @test@ subdirectory.
check :: Int                  -- ^ The number of inputs to generate.
      -> Module               -- ^ The defining module.
      -> Def (args :-> IBool) -- ^ The property to check.
      -> IO ()
check n m prop@(DefProc p) = do
  inputs <- sampleProc m p n
  let main = DefProc I.Proc { I.procSym = "main"
                            , I.procRetTy = I.TyVoid
                            , I.procArgs = []
                            , I.procBody = concat inputs
                            , I.procRequires = []
                            , I.procEnsures = []
                            }
  let test = package (I.modName m ++ "__test") $ do
               depend m
               incl prop
               incl main
  runCompiler [m, test] [] initialOpts { outDir = Just "test" }
check _ _ _ = error "I can only check normal Ivory procs!"

-- | Make a @check@able property from an arbitrary Ivory procedure. The
-- property will simply check that the contracts are satisfied.
contract :: Def (args :-> res) -> Def (args :-> IBool)
contract (DefProc (I.Proc {..}))
  = DefProc I.Proc
      { I.procSym = procSym ++ "__contract_check"
      , I.procRetTy = I.TyBool
      , I.procArgs = procArgs
      , I.procBody = [ I.Call procRetTy Nothing (I.NameSym procSym)
                       [ I.Typed t (I.ExpVar v) | I.Typed t v <- procArgs ]
                     , I.Return (I.Typed I.TyBool (I.ExpLit (I.LitBool True)))
                     ]
      , I.procRequires = procRequires
      , I.procEnsures = []
      }
contract _ = error "I can only check contracts of normal Ivory procs!"

mkUnique :: (?counter :: IORef Integer) => IO Integer
mkUnique = do
  i <- readIORef ?counter
  writeIORef ?counter (i+1)
  return i

sampleProc :: Module -> I.Proc -> Int -> IO [I.Block]
sampleProc m@(I.Module {..}) p@(I.Proc {..}) n
  = do c <- newIORef 0
       let ?counter = c
       initss <- forM procArgs $ \ (I.Typed t _) ->
         sampleType m t n
       forM (transpose initss) $ \ args -> do
         let (vars, inits) = unzip args
         chk <- mkCheck p vars
         return (concat inits ++ chk)

mkCheck :: (?counter :: IORef Integer)
        => I.Proc -> [I.Var] -> IO I.Block
mkCheck (I.Proc {..}) args = do
  n <- mkUnique
  let b = mkVar n
  let c = [ I.Call I.TyBool (Just b) (I.NameSym procSym)
            [ I.Typed t (I.ExpVar v) | (I.Typed t _, v) <- zip procArgs args ]
          , I.Assert (I.ExpVar b) ]
  return c

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
  I.TyRef ty
    -> mapM (mkRef ty) =<< sampleType m ty n
  I.TyConstRef ty
    -> mapM (mkRef ty) =<< sampleType m ty n
  I.TyArr len ty
    -> sampleArray m len ty n
  I.TyStruct ty
    -> sampleStruct m ty n
  I.TyProc _ _ -> err 
  I.TyVoid     -> err 
  I.TyPtr _    -> err 
  I.TyCArray _ -> err 
  I.TyOpaque   -> err
  where
  err = error $ "I don't know how to make values of type '" ++ show t ++ "'!"

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
  return (r, init ++ [ I.AllocRef ty r (I.NameVar v) ])

mkVar :: Integer -> I.Var
mkVar n = I.VarName ("var" ++ show n)

mk :: Int -> G.Gen a -> IO [a]
mk n g = G.generate (G.vectorOf n g)

sampleInt :: I.IntSize -> Int -> IO [I.Init]
sampleInt sz n = do
  xs <- gen
  return [ I.InitExpr (I.TyInt sz) (I.ExpLit (I.LitInteger x)) | x <- xs ]
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
  return [ I.InitExpr (I.TyIndex ix)
           (I.ExpLit (I.LitInteger (fromIntegral x `mod` ix)))
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

sampleStruct :: (?counter :: IORef Integer)
             => I.Module -> String -> Int -> IO [(I.Var, I.Block)]
sampleStruct m@(I.Module {..}) ty n
  = case find (\s -> ty == I.structName s) structs of
    Just (I.Struct _ fields) -> replicateM n $ do
      (vars, blcks) <- fmap unzip $ forM fields $ \ (I.Typed t _) ->
        fmap head $ sampleType m t 1
      let init = zipWith (\ (I.Typed t f) v -> (f, I.InitExpr t (I.ExpVar v)))
                 fields vars
      (v, blck) <- mkLocal (I.TyStruct ty) (I.InitStruct init)
      return (v, concat blcks ++ blck)
    _ -> error ("I don't know how to construct a '" ++ ty ++ "'!")
  where
  structs = I.public modStructs ++ I.private modStructs

sampleArray :: (?counter :: IORef Integer)
            => I.Module -> Int -> I.Type -> Int
            -> IO [(I.Var, I.Block)]
sampleArray m len ty n = replicateM n $ do
  (vars, blcks) <- fmap unzip $ replicateM len (fmap head $ sampleType m ty 1)
  let init = [ I.InitExpr ty (I.ExpVar v) | v <- vars ]
  (v, blck) <- mkLocal (I.TyArr len ty) (I.InitArray init)
  return (v, concat blcks ++ blck)
