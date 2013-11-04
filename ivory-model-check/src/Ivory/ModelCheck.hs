{-# LANGUAGE OverloadedStrings #-}

module Ivory.ModelCheck where

import qualified Ivory.Language.Syntax as I
import qualified Ivory.ModelCheck.Ivory2CVC4 as C
import qualified Ivory.ModelCheck.Monad
import qualified Ivory.ModelCheck.CVC4

import           System.FilePath
import qualified Data.ByteString.Char8 as B

--------------------------------------------------------------------------------

data Args = Args
  { printQuery  :: Bool
  , callCVC4    :: Bool
  , cvc4Path    :: FilePath
  , modules     :: [I.Module]
  , cvc4Args    :: [String]
  } deriving (Show, Eq)

modelCheck :: I.Module -> IO ()
modelCheck m = do
  (_, st) <- runMC (modelCheck m)
  let script = mkScript st

--   let (_, st) = runMC (modelCheck' m)
--   putStrLn $ "\n  ** Env: \n"
--   putStrLn $ show (symEnv st)

--   putStrLn $ "\n  ** Queries: \n"
--   wrExps (assertQueries $ symQuery st) query

--   let ps = symSt st
--   putStrLn $ "\n  ** Decls: \n"
--   mapM_ (B.putStrLn . concrete) (reverse $ decls ps)
--   wrExps (eqns ps) assert

--   where
--   wrExps exps f = mapM_ (B.putStrLn . concrete . f) (reverse $ exps)

mkScript :: SymExecSt -> [B.ByteString]
mkScript st =
  [ "% Script auto-generated for model-checking Ivory function "
  ++ B.pack funcSym
  , ""
  , "% declarations -------------------------------"
  , ""
  ] ++ writeStmts  (concrete . decls . symSt)
  ++
  [ ""
  , "% program encoding ---------------------------"
  , ""
  ] ++ writeStmts (assert . eqns . symSt)
  ++
  [""
  , "% queries ------------------------------------"
  , ""
  ] ++ writeStmts (assertQueries . symQuery)


  where
  writeStmts bs f = map (concrete . reverse . f) st
