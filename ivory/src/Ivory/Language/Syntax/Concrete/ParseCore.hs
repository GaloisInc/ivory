{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--
-- Ivory concrete syntax core parser functions.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.Syntax.Concrete.ParseCore where

import Prelude ()
import Prelude.Compat

import MonadLib
import qualified Text.PrettyPrint as P

import Ivory.Language.Syntax.Concrete.Location
import Ivory.Language.Syntax.Concrete.Lexeme
import Ivory.Language.Syntax.Concrete.Pretty

--------------------------------------------------------------------------------

data ParserState = ParserState { psTokens  :: [Lexeme]
                               } deriving Show

initParserState :: [Lexeme] -> ParserState
initParserState ls =
  ParserState { psTokens  = ls
              }

newtype Parser a = Parser
  { unParser :: StateT ParserState Id a
  } deriving (Functor,Applicative,Monad)

-- | Run the parser over the file given.
runParser :: [Lexeme] -> Parser a -> a
runParser ls m =
  fst $ runId $ runStateT (initParserState ls) (unParser m)

lexer :: (Lexeme -> Parser a) -> Parser a
lexer k =
  do ps <- Parser get
     case psTokens ps of
       l:ls -> do Parser (set ps { psTokens = ls })
                  k l
       []   -> error "Unexpected end of input"

parseError :: Lexeme -> Parser a
parseError l =
  error $ prettyPrint
        $ P.text "Parser error near" P.<+> pretty (getLoc l)
          P.<+> P.text "for" P.$+$ P.nest 2 (P.text (show (unLoc l)))
