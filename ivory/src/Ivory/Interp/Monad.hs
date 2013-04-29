{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Interp.Monad (
    -- * Evaluation Monad
    Eval(..)
  , Env(..)
  , Frame(..)
  , Cont(..)

    -- ** Primitive Actions
  , io
  , evalContext
  , embed

    -- ** Environment Management
  , setResult
  , getResult
  , addProc
  , lookupProc
  , addStruct
  , lookupStruct
  , clearStack

    -- ** Frame Management
  , pushFrame, popFrame
  , modifyFrame, modifyFrame_
  , queryFrame
  , bindLocal, bindArg, bindResult
  , lookupLocal
  , getStmt

    -- ** Continuation Management
  , pushCont
  , popCont
  ) where

import Ivory.Interp.Error (frameStackEmpty,contStackEmpty,unboundLocal)
import Ivory.Interp.Value (Value)
import Ivory.Language.Syntax
    (Block,Proc(..),Stmt,Var,Sym,Typed(..),Struct,structName)

import Control.Applicative (Applicative,(<$))
import Control.Monad (void)
import Data.IORef (IORef,newIORef,readIORef,writeIORef)
import MonadLib (ReaderT,inBase,ask,runM)
import qualified Data.Map as Map


-- Evaluation Monad ------------------------------------------------------------

-- | Evaluation in context.
newtype Eval a = Eval
  { unEval :: ReaderT (IORef Env) IO a
  } deriving (Functor,Applicative,Monad)


-- Environment -----------------------------------------------------------------

data Env = Env
  { envFrames :: [Frame]
  , envResult :: Maybe Value
  , envProcs  :: Map.Map Sym Proc
  , envStructs :: Map.Map String Struct
  }

-- | An empty environment.
emptyEnv :: Env
emptyEnv  = Env
  { envFrames  = []
  , envResult  = Nothing
  , envProcs   = Map.empty
  , envStructs = Map.empty
  }


-- Frames ----------------------------------------------------------------------

-- | A stack frame, representing the context of an executing function.
data Frame = Frame
  { frameLocals :: Map.Map Var (Typed Value)
  , frameStmts  :: Block
  , frameConts  :: [Cont] -- ^ Break path
  , frameResult :: Maybe Var
  }

-- | An empty stack frame.
emptyFrame :: Frame
emptyFrame  = Frame
  { frameLocals = Map.empty
  , frameStmts  = []
  , frameConts  = []
  , frameResult = Nothing
  }

hasCont :: Frame -> Bool
hasCont  = not . null . frameConts


-- Frame Continuations ---------------------------------------------------------

-- | Frame continuations.  These occur when control transfers within a frame,
-- causing a new instruction stack to be pushed, and the current one to be
-- saved, with a restore action, on the continuation stack.
data Cont = Cont
  { contRestore :: Eval () -- ^ Continuation restore action
  , contStmts   :: Block   -- ^ Block to continue executing
  }


-- Primitive Actions -----------------------------------------------------------

-- | Run an 'Eval' action.
evalContext :: IO (Eval a -> IO a)
evalContext  = do
  env <- newIORef emptyEnv
  return (\m -> runM (unEval m) env)

embed :: Eval a -> Eval (IO a)
embed m = Eval (runM (unEval m) `fmap` ask)

-- | Perform some IO in the context of the Eval monad.
io :: IO a -> Eval a
io m = Eval (inBase m)


-- Environment Management ------------------------------------------------------

clearStack :: IORef Env -> IO ()
clearStack ref = do
  env <- readIORef ref
  writeIORef ref env { envFrames = [] }

-- XXX don't export
get :: Eval Env
get  = Eval (inBase . readIORef =<< ask)

set :: Env -> Eval ()
set env = Eval (inBase . (`writeIORef` env) =<< ask)

-- | Set the value of the result register.
setResult :: Maybe Value -> Eval ()
setResult mb = do
  env <- get
  set env { envResult = mb }

-- | Get the value of the result register.
getResult :: Eval (Maybe Value)
getResult  = envResult `fmap` get

-- | Add a 'Proc' to the current environment.
addProc :: Proc -> Eval ()
addProc def = do
  env <- get
  set env { envProcs = Map.insert (procSym def) def (envProcs env) }

-- | Add a 'Struct' to the current environment.
addStruct :: Struct -> Eval ()
addStruct def = do
  env <- get
  set env { envStructs = Map.insert (structName def) def (envStructs env) }

-- | Lookup a 'Proc' in the current environment.
lookupProc :: Sym -> Eval Proc
lookupProc sym = do
  env <- get
  case Map.lookup sym (envProcs env) of
    Just def -> return def
    Nothing  -> fail ("lookupProc: unknown symbol: " ++ sym)

lookupStruct :: String -> Eval Struct
lookupStruct name = do
  env <- get
  case Map.lookup name (envStructs env) of
    Just def -> return def
    Nothing  -> fail ("lookupStruct: unknown struct: " ++ name)


-- Frame Management ------------------------------------------------------------

-- | Push a new code frame.
pushFrame :: Maybe Var -> Block -> Eval ()
pushFrame mbR code = do
  env <- get
  let frame = emptyFrame
        { frameResult = mbR
        , frameStmts  = code
        }
  set env { envFrames = frame : envFrames env }

-- | Pop the current code frame, returning the optional name to bind in the
-- calling scope.
popFrame :: Eval (Maybe Var)
popFrame  = do
  env <- get
  case envFrames env of
    frame : rest -> do
      set env { envFrames = rest }
      return (frameResult frame)

    [] -> io (frameStackEmpty "popFrame")

-- | Modify the current stack frame.
modifyFrame :: (Frame -> (a,Frame)) -> Eval a
modifyFrame update = do
  env <- get
  case envFrames env of

    f:rest -> do
      let (a,f') = update f
      set env { envFrames = f' : rest }
      return a

    [] -> io (frameStackEmpty "modifyFrame")

-- | Modify the current stack frame, but don't return a value.
modifyFrame_ :: (Frame -> Frame) -> Eval ()
modifyFrame_ f = void $ modifyFrame $ \ frame ->
  let f' = f frame in (f' `seq` (), f')

-- | Query the current stack frame.
queryFrame :: (Frame -> a) -> Eval a
queryFrame f = modifyFrame $ \ frame -> (f frame, frame)

-- | Bind a local value in the current stack frame.
bindLocal :: Var -> Typed Value -> Eval ()
bindLocal n tv = modifyFrame_ $ \ frame -> frame
  { frameLocals = Map.insert n tv (frameLocals frame)
  }

-- | Bind an argument in the current frame.  Used when setting up a new frame.
bindArg :: Typed Var -> Value -> Eval ()
bindArg tn v = bindLocal (tValue tn) (v <$ tn)

-- | Bind a result in the calling frame, if a name was given.
bindResult :: Typed Value -> Maybe Var -> Eval ()
bindResult tv mb = case mb of
  Just var -> bindLocal var tv
  Nothing  -> return ()

-- | Lookup the value of a variable in the current scope.
lookupLocal :: Var -> Eval Value
lookupLocal n = do
  mb <- queryFrame (Map.lookup n . frameLocals)
  case mb of
    Just val -> return (tValue val)
    Nothing  -> io (unboundLocal n)

stackDepth :: Eval Int
stackDepth  = (length . envFrames) `fmap` get

-- | Get an instruction from the statement stack, popping the continuation stack
-- if there's nothing left.
getStmt :: Eval (Maybe Stmt)
getStmt  = do
  depth <- stackDepth
  if depth > 0
     then retrieve
     else return Nothing

  where
  retrieve = do
    mb <- modifyFrame $ \ frame -> case frameStmts frame of
      s:rest -> (Left s,frame { frameStmts = rest })
      _      -> (Right (hasCont frame), frame)

    case mb of
      Left stmt         -> return (Just stmt)
      Right cont | cont -> popCont >> getStmt
      _                 -> return Nothing


-- Continuation Management -----------------------------------------------------

-- | Push a new set of instructions, and save the current IP with a restore
-- action.
pushCont :: Block -> Eval () -> Eval ()
pushCont code enter = modifyFrame_ $ \ frame -> frame
  { frameStmts = code
  , frameConts = Cont enter (frameStmts frame) : frameConts frame
  }

-- | Pop the continuation stack, running its action, and restoring the 
popCont :: Eval ()
popCont  = do
  mb <- modifyFrame $ \ frame -> case frameConts frame of
    k : rest -> (Just k,frame { frameConts = rest, frameStmts = contStmts k })
    _        -> (Nothing, frame)
  case mb of
    Just k  -> contRestore k
    Nothing -> io contStackEmpty
