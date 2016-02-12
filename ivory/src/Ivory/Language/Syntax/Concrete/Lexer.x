-- # -*- mode: haskell -*-
{

{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-tabs -fno-warn-unused-matches -fno-warn-unused-imports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--
-- Ivory lexer.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--
-- Lexer.hs is generated!

module Ivory.Language.Syntax.Concrete.Lexer where

import Prelude ()
import Prelude.Compat

import Data.Char (ord)
import Data.Word (Word8)
import Data.Bits (shiftR,(.&.))
import MonadLib
import qualified Data.Text.Lazy as L

import Ivory.Language.Syntax.Concrete.Lexeme
import Ivory.Language.Syntax.Concrete.Location

}

--------------------------------------------------------------------------------

$digit       = [0-9]
$hexdig      = [0-9A-Fa-f]
$alpha       = [a-zA-Z]
$lowerletter = [a-z]
$capletter   = [A-Z]

@sym         = [\/ \* \+ \- \= \< \> \! \% \| \& \^ \~ \? \: \# \_ \. \$ \@]+
@tyident     = $capletter   [$alpha $digit [_ \']]*
@ident       = $lowerletter [$alpha $digit [_ \']]*
@brack       = [\( \) \[ \] \{ \}]
@sep         = [\, \;]
-- @filepath    = \" [$printable # \" # $white]+ \"
@string      = \" [$printable # \"]* \"
@bitlit      = $digit+ b [0 1]+
@hexlit      = 0x $hexdig+
@float       = $digit+ \. $digit+

--------------------------------------------------------------------------------

tokens :-
  $white+ ;
  "--".*  ;

  $digit+      { emitS readInteger }
  @hexlit      { emitS readInteger }
  @bitlit      { emitS readBitLit }
  @float       { emitS readFloat }

-- Reserved words: statements
  if       { keyword }
  else     { keyword }
  assert   { keyword }
  assume   { keyword }
  pre      { keyword }
  post     { keyword }
  let      { keyword }
  return   { keyword }
  alloc    { keyword }
  store    { keyword }
  as       { keyword }
  map      { keyword }
  upTo     { keyword }
  forever  { keyword }
  break    { keyword }

-- Reserved words: expressions
  abs              { keyword }
  signum           { keyword }
  exp              { keyword }
  sqrt             { keyword }
  log              { keyword }
  pow              { keyword }
  div              { keyword }

  sin              { keyword }
  cos              { keyword }
  tan              { keyword }

  asin             { keyword }
  acos             { keyword }
  atan             { keyword }
  atan2            { keyword }

  sinh             { keyword }
  cosh             { keyword }
  tanh             { keyword }

  asinh            { keyword }
  acosh            { keyword }
  atanh            { keyword }

  isnan            { keyword }
  isinf            { keyword }
  round            { keyword }
  ceil             { keyword }
  floor            { keyword }
  const            { keyword }

  memcpy           { keyword }

  safeCast         { keyword }
  bitCast          { keyword }
  castWith         { keyword }
  twosCompCast     { keyword }
  twosCompRep      { keyword }

  fromIx           { keyword }
  ixSize           { keyword }
  toIx             { keyword }
  toCArray         { keyword }
  arrayLen         { keyword }

  sizeOf           { keyword }
  nullPtr          { keyword }
  refToPtr         { keyword }

-- Reserved words
  struct   { keyword }
  abstract { keyword }
  string   { keyword }
  type     { keyword }
  include  { keyword }
  import   { keyword }

  -- C style
  bool     { keyword }
  char     { keyword }
  float    { keyword }
  double   { keyword }
  void     { keyword }

  int8_t   { keyword }
  int16_t  { keyword }
  int32_t  { keyword }
  int64_t  { keyword }

  uint8_t  { keyword }
  uint16_t { keyword }
  uint32_t { keyword }
  uint64_t { keyword }

  ix_t     { keyword }

  S        { keyword }
  G        { keyword }

  -- Haskell style
  Stack    { keyword }
  Global   { keyword }

  IBool    { keyword }
  IChar    { keyword }
  IFloat   { keyword }
  IDouble  { keyword }

  IString  { keyword }

  Sint8    { keyword }
  Sint16   { keyword }
  Sint32   { keyword }
  Sint64   { keyword }

  Uint8    { keyword }
  Uint16   { keyword }
  Uint32   { keyword }
  Uint64   { keyword }

  Ref      { keyword }
  ConstRef { keyword }
  Array    { keyword }
  Struct   { keyword }
  Stored   { keyword }

  Ix       { keyword }

  -- Bit data
  bitdata  { keyword }
  Bit      { keyword }
  Bits     { keyword }
  BitArray { keyword }

-- Identifiers
  @ident    { emitS TokIdent }
-- Type Identifiers
  @tyident  { emitS TokTyIdent }
-- Symbols (match if it's not a reserved word)
  @sym      { emitS TokSym }
-- Brackets
  @brack    { emitS TokBrack }
-- Separators
  @sep      { emitS TokSep }
-- Strings
  @string { emitS readStringLit }

--------------------------------------------------------------------------------

{

type AlexInput = LexerInput

data LexerInput = LexerInput
  { liPosn   :: !Position
  , liSource :: FilePath
  , liChar   :: !Char
  , liBytes  :: [Word8]
  , liInput  :: L.Text
  } deriving (Show)

initLexerInput :: FilePath -> L.Text -> LexerInput
initLexerInput source bytes = LexerInput
  { liPosn   = Position 0 1 1
  , liSource = source
  , liChar   = '\n'
  , liBytes  = []
  , liInput  = bytes
  }

-- | Build a range from the lexer state.
mkRange :: LexerInput -> String -> SrcLoc
mkRange li str =
  SrcLoc (Range (liPosn li) (movesPos (liPosn li) str)) (Just (liSource li))

fillBuffer :: LexerInput -> Maybe LexerInput
fillBuffer li = do
  (c,rest) <- L.uncons (liInput li)
  return $! li
    { liPosn  = movePos (liPosn li) c
    , liBytes = utf8Encode c
    , liChar  = c
    , liInput = rest
    }

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar  = liChar

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte li = case liBytes li of
  b:bs -> return (b, li { liBytes = bs })
  _    -> alexGetByte =<< fillBuffer li

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 +  oc .&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + ( oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 +   oc             .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + ( oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6)  .&. 0x3f)
                        , 0x80 +   oc              .&. 0x3f
                        ]


-- Lexer Monad -----------------------------------------------------------------

newtype Lexer a = Lexer
  { unLexer :: StateT LexerState Id a
  } deriving (Functor,Monad,Applicative)

instance StateM Lexer LexerState where
  get = Lexer   get
  set = Lexer . set

data LexerState = LexerState
  { lexerInput :: !LexerInput
  , lexerState :: !Int
  } deriving Show

scan :: FilePath -> L.Text -> [Lexeme]
scan source bytes = fst (runId (runStateT st0 (unLexer loop)))
  where
  st0  = LexerState
    { lexerInput = initLexerInput source bytes
    , lexerState = 0
    }

  loop = do
    inp <- alexGetInput
    sc  <- alexGetStartCode
    case alexScan inp sc of

      AlexToken inp' len action -> do
        alexSetInput inp'
        mb   <- action inp len
        rest <- loop
        case mb of
          Just l   -> return (l:rest)
          Nothing  -> return rest

      AlexSkip inp' _len -> do
        alexSetInput inp'
        loop

      AlexEOF ->
        return [Located mempty TokEOF]

      AlexError inp' ->
        return [Located (mkRange inp' "") (TokError "Lexical error")]

alexSetInput :: AlexInput -> Lexer ()
alexSetInput inp = do
  st <- get
  set $! st { lexerInput = inp }

alexGetInput :: Lexer AlexInput
alexGetInput  = lexerInput `fmap` get


-- Start Codes -----------------------------------------------------------------

alexGetStartCode :: Lexer Int
alexGetStartCode  = lexerState `fmap` get

alexSetStartCode :: Int -> Lexer ()
alexSetStartCode code = do
  s <- get
  set $! s { lexerState = code }

-- Actions ---------------------------------------------------------------------

type AlexAction result = AlexInput -> Int -> result

-- | Emit a token from the lexer
emitT :: Token -> AlexAction (Lexer (Maybe Lexeme))
emitT tok = emitS (const tok)

emitS :: (String -> Token) -> AlexAction (Lexer (Maybe Lexeme))
emitS mk li len = return (Just $! Located (mkRange li str) (mk str))
  where
  str = L.unpack (L.take (fromIntegral len) (liInput li))

keyword :: AlexAction (Lexer (Maybe Lexeme))
keyword = emitS TokReserved

begin :: Int -> AlexAction (Lexer (Maybe Lexeme))
begin code _ _ = alexSetStartCode code >> return Nothing

}

