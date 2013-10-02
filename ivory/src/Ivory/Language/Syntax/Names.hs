{-# LANGUAGE TemplateHaskell #-}

module Ivory.Language.Syntax.Names where

import Language.Haskell.TH.Lift (deriveLiftMany)


-- Public Symbols --------------------------------------------------------------

-- | Symbol names.
type Sym = String


-- Names -----------------------------------------------------------------------

-- | Variable names.
data Var
  = VarName String
    -- ^ Names
  | VarInternal String
    -- ^ Internal names
  | VarLitName String
    -- ^ A literal name that should not be mangled
    deriving (Show,Eq,Ord)


-- Special Names ---------------------------------------------------------------

-- | The name for the return value named in an ensures statement.
retval :: Var
retval  = VarInternal "retval"


-- TH Lifting ------------------------------------------------------------------

deriveLiftMany [ ''Var ]
