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
module Ivory.QuickCheck.Testable where

import           Control.Monad
import           Ivory.Compile.C.CmdlineFrontend
import           Ivory.Language
import           Ivory.Language.Proc
import qualified Ivory.Language.Syntax           as I
import           Ivory.QuickCheck

import qualified GHC.Exts                        as GHC

import qualified Test.QuickCheck.Arbitrary       as A
import qualified Test.QuickCheck.Gen             as G

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

class Testable p where
  sample :: Def p -> Int -> IO [HList (Args p)]
  apply :: Def p -> HList (Args p) -> Ivory eff IBool

instance Testable ('[] :-> IBool) where
  sample _ n = replicateM n (return Nil)
  apply p Nil = call p

instance (IvoryVar a, A.Arbitrary a) => Testable ('[a] :-> IBool) where
  sample _ n = runIO $ do
    as <- samples n (A.arbitrary :: G.Gen a)
    return $ map (::: Nil) as
  apply p (a ::: Nil) = call p a

instance ( IvoryVar a, A.Arbitrary a
         , IvoryVar b, A.Arbitrary b
         ) => Testable ('[a,b] :-> IBool) where
  sample _ n = runIO $ do
    as <- samples n (A.arbitrary :: G.Gen a)
    bs <- samples n (A.arbitrary :: G.Gen b)
    return $ zipWith (\a b -> a ::: b ::: Nil) as bs
  apply p (a ::: b ::: Nil) = call p a b

instance ( IvoryVar a, A.Arbitrary a
         , IvoryVar b, A.Arbitrary b
         , IvoryVar c, A.Arbitrary c
         ) => Testable ('[a,b,c] :-> IBool) where
  sample _ n = runIO $ do
    as <- samples n (A.arbitrary :: G.Gen a)
    bs <- samples n (A.arbitrary :: G.Gen b)
    cs <- samples n (A.arbitrary :: G.Gen c)
    return $ zipWith3 (\a b c -> a ::: b ::: c ::: Nil) as bs cs
  apply p (a ::: b ::: c ::: Nil) = call p a b c

check :: Testable p => Int -> Module -> Def p -> IO ()
check n m prop@(DefProc p) = do
  inputs <- sample prop n
  let main = proc "main" $ body $ do
               forM_ inputs $ \i -> do
                 assert =<< apply prop i
               retVoid
  let test = package (I.modName m ++ "__test") $ do
               depend m
               incl prop
               incl main
  runCompiler [m, test] [] initialOpts { outDir = Just "test" }
