{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ivory.Interp.Value where

import Ivory.Interp.Error
import Ivory.Language.Float
import Ivory.Language.IBool
import Ivory.Language.IChar
import Ivory.Language.Proxy
import Ivory.Language.Sint
import Ivory.Language.Syntax.Names
import Ivory.Language.Type
import Ivory.Language.Uint

import Control.Exception (throw)
import Data.IORef (newIORef,IORef,readIORef,writeIORef)
import Data.Int (Int8,Int16,Int32,Int64)
import Data.Word (Word8,Word16,Word32,Word64)
import qualified Data.Traversable as T


class IvoryVar i => FromValue i t | i -> t where
  fromValue :: Proxy i -> Value -> t

instance FromValue IChar Char where
  fromValue _ val = case val of
    ValChar c -> c
    _         -> throw (TypeError "fromValue" "expected a char")

instance FromValue IBool Bool where
  fromValue _ val = case val of
    ValBool b -> b
    _         -> throw (TypeError "fromValue" "expected a bool")

instance FromValue Sint8 Int8 where
  fromValue _ = extractInt

instance FromValue Sint16 Int16 where
  fromValue _ = extractInt

instance FromValue Sint32 Int32 where
  fromValue _ = extractInt

instance FromValue Sint64 Int64 where
  fromValue _ = extractInt

instance FromValue Uint8 Word8 where
  fromValue _ = extractInt

instance FromValue Uint16 Word16 where
  fromValue _ = extractInt

instance FromValue Uint32 Word32 where
  fromValue _ = extractInt

instance FromValue Uint64 Word64 where
  fromValue _ = extractInt

instance FromValue IFloat Float where
  fromValue _ = extractFloat

instance FromValue IDouble Double where
  fromValue _ = extractDouble



-- Values ----------------------------------------------------------------------

-- | Values for the interpreter.  As an aside, most operations on values are
-- partial, though this is expected to not be a problem, as we should only be
-- evaluating Ivory AST that's made it through the typechecker.
data Value
  = ValNull
  | ValBool   !Bool
  | ValInt    !Integer
  | ValFloat  !Float
  | ValDouble !Double
  | ValChar   !Char
  | ValStr    String
  | ValSym    Sym
  | ValRef    (IORef Value)
  | ValPtr    (Maybe (IORef Value))
  | ValStruct [(String,IORef Value)]
  | ValArray  Int [IORef Value]
    deriving (Eq)

fromInt :: Integral a => a -> Value
fromInt  = ValInt . toInteger

fromFloat :: Float -> Value
fromFloat  = ValFloat

fromDouble :: Double -> Value
fromDouble  = ValDouble

instance Show Value where
  showsPrec p val = case val of
    ValNull       ->          showString "ValNull"
    ValBool b     -> parens $ showString "ValBool " . shows b
    ValInt i      -> parens $ showString "ValInt " . shows i
    ValFloat f    -> parens $ showString "ValFloat " . shows f
    ValDouble d   -> parens $ showString "ValDouble " . shows d
    ValChar c     -> parens $ showString "ValChar " . shows c
    ValStr s      -> parens $ showString "ValStr " . shows s
    ValSym sym    -> parens $ showString "ValSym " . showString sym
    ValRef _      -> parens $ showString "ValRef <IORef>"
    ValPtr mb     -> parens $ case mb of
      Just _      -> showString "ValPtr <IORef>"
      Nothing     -> showString "ValPtr <Null>"
    ValStruct _   -> parens $ showString "ValStruct <fields>"
    ValArray n _  -> parens $ showString "ValArray " . shows n . showString " <elems>"
    where
    parens = showParen (p >= 10)


-- Booleans --------------------------------------------------------------------

-- | Test if a value is true.
isTrue :: Value -> Bool
isTrue val = case val of
  ValBool b -> b
  _         -> throw (TypeError "isTrue" "expected a boolean")

-- | Test if a value is false.
isFalse :: Value -> Bool
isFalse val = case val of
  ValBool b -> not b
  _         -> throw (TypeError "isFalse" "expected a boolean")


-- Numbers ---------------------------------------------------------------------

extractInt :: Num a => Value -> a
extractInt val = case val of
  ValInt i -> fromInteger i
  _        -> throw (TypeError "extractInt" "expected an integer")

extractFloat :: Value -> Float
extractFloat val = case val of
  ValFloat f -> f
  ValInt i   -> fromInteger i
  _          -> throw (TypeError "extractFloat" "expected a float")

extractDouble :: Value -> Double
extractDouble val = case val of
  ValDouble f -> f
  ValInt i    -> fromInteger i
  _           -> throw (TypeError "extractFloat" "expected a double")


-- References ------------------------------------------------------------------

newRef :: Value -> IO Value
newRef v = ValRef `fmap` newIORef v

-- | Run a computation with a reference.
withRef :: Value -> (IORef Value -> IO a) -> IO a
withRef val k = case val of
  ValRef ref -> k ref
  _          -> typeError "withRef" "expected a reference"

-- | Read a value from a reference.
readRef :: Value -> IO Value
readRef val = withRef val readIORef

-- | Write a value to a reference.
writeRef :: Value -> Value -> IO ()
writeRef ref val = withRef ref (`writeIORef` val)


-- Pointers --------------------------------------------------------------------

newPtr :: Maybe Value -> IO Value
newPtr mb = ValPtr `fmap` T.sequenceA (newIORef `fmap` mb)
