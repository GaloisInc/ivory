{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeOperators #-}

module Ivory.Opts.CFG
  ( callGraphDot
  -- ** Generate a dot file of the control flow for a program.
  , SizeMap(..)
  -- ** Implementation-defined size map for stack elements.
  , defaultSizeMap
  , hasLoop
  -- ** Does the control-flow graph contain a loop?
  , WithTop
  , maxStack
  -- ** What is the maximum stack size of the program?
  )
    where

import Prelude ()
import Prelude.Compat hiding (lookup)

import qualified Ivory.Language.Array       as I
import qualified Ivory.Language.Syntax.AST  as I
import qualified Ivory.Language.Syntax.Type as I
import qualified Data.Graph.Inductive as G

import Control.Applicative (liftA2)
import System.FilePath
import Data.Maybe
import Data.List (find,(\\))
import qualified Data.IntMap as M
import MonadLib (StateT, get, set, Id, StateM, runM)
import MonadLib.Derive (derive_get, derive_set, Iso(..))

-- import Ivory.Language hiding (Top)

--------------------------------------------------------------------------------
-- Types

-- | Add Top to a type.
data WithTop a = Top | Val a
  deriving (Eq, Functor)

instance Show a => Show (WithTop a) where
  show Top     = "Top"
  show (Val a) = show a

instance Ord a => Ord (WithTop a) where
  compare Top     (Val _) = GT
  compare (Val _) Top     = LT
  compare Top      Top    = EQ
  compare (Val a) (Val b) = compare a b

instance Applicative WithTop where
  pure a = Val a
  Val _ <*> Top     = Top
  Val f <*> (Val a) = Val (f a)
  _     <*> _       = Top

type Size = Integer
type CallNm = String

-- | We track those objects for which the size might be
-- implementation-dependent.
data StackType = TyStruct String      -- ^ Name of struct
               | TyArr StackType Size -- ^ Array and size
               | Ptr                  -- ^ Pointer or ref type
               | TyVoid
               | TyInt IntSize
               | TyWord WordSize
               | TyBool
               | TyChar
               | TyFloat
               | TyDouble
  deriving (Show, Eq)

data IntSize = Int8
             | Int16
             | Int32
             | Int64
  deriving (Show,Eq)

data WordSize = Word8
              | Word16
              | Word32
              | Word64
  deriving (Show,Eq)

data (Show a, Eq a) => Block a
  = Stmt a
  | Branch [Block a] [Block a]
  | Loop (Maybe Integer) [Block a] -- If we know how many loops we make, we
                                   -- store it.  Otherwise, `Nothing`.
  deriving (Show, Eq)

type Control = Block CallNm    -- ^ Name of function being called
type Alloc   = Block StackType -- ^ Memory being allocated

-- | Describes the CFG and memory usage for a single procedure.
data ProcInfo = ProcInfo
  { procSym :: CallNm
  , params  :: [StackType]
    -- ^ Parameters pushed onto the stack.
  , alloc   :: [Alloc]
    -- ^ Allocated elements.
  , calls   :: [Control]
  } deriving (Show, Eq)

data ModuleInfo = ModuleInfo
  { modName :: String
  , procs   :: [ProcInfo]
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Procedure Analysis

toStackTyped :: I.Typed a -> StackType
toStackTyped ty = toStackType (I.tType ty)

toStackType :: I.Type -> StackType
toStackType ty =
  case ty of
    I.TyStruct nm  -> TyStruct nm
    I.TyArr i t    -> TyArr (toStackType t) (fromIntegral i)
    I.TyRef{}      -> Ptr
    I.TyConstRef{} -> Ptr
    I.TyPtr{}      -> Ptr
    I.TyVoid       -> TyVoid
    I.TyInt i      -> TyInt (toIntType i)
    I.TyWord w     -> TyWord (toWordType w)
    I.TyIndex _n   -> toStackType I.ixRep
    I.TyBool       -> TyBool
    I.TyChar       -> TyChar
    I.TyFloat      -> TyFloat
    I.TyDouble     -> TyDouble
    t              -> error $ "Unhandled stack type: " ++ show t

toIntType :: I.IntSize -> IntSize
toIntType i =
  case i of
    I.Int8  -> Int8
    I.Int16 -> Int16
    I.Int32 -> Int32
    I.Int64 -> Int64

toWordType :: I.WordSize -> WordSize
toWordType i =
  case i of
    I.Word8  -> Word8
    I.Word16 -> Word16
    I.Word32 -> Word32
    I.Word64 -> Word64

cfgProc :: I.Proc -> ProcInfo
cfgProc proc = ProcInfo
  { procSym = I.procSym proc
  , params  = map toStackTyped  (I.procArgs proc)
  , alloc   = concatMap toAlloc (I.procBody proc)
  , calls   = concatMap toCall  (I.procBody proc)
  }

toAlloc :: I.Stmt -> [Alloc]
toAlloc stmt =
  case stmt of
    I.Assign ty _ _                     -> [Stmt $ toStackType ty]
    I.AllocRef ty _ _                   -> [Stmt $ toStackType ty]
    I.Deref ty _ _                      -> [Stmt $ toStackType ty]
    -- Descend into blocks
    I.IfTE _ blk0 blk1                  -> [ Branch (concatMap toAlloc blk0)
                                                    (concatMap toAlloc blk1) ]
                                           -- For the loop variable.
    I.Loop m _ e _ blk                  ->
      let ty = I.ixRep in
      [Stmt (toStackType ty), Loop (Just (loopIdx m e)) (concatMap toAlloc blk)]
    I.Forever blk                       ->
      [Loop Nothing (concatMap toAlloc blk)]
    _                                   -> []

toCall :: I.Stmt -> [Control]
toCall stmt =
  case stmt of
    I.IfTE _ blk0 blk1 -> [ Branch (concatMap toCall blk0)
                                   (concatMap toCall blk1) ]
    I.Call _ _ nm _  -> case nm of
                          I.NameSym sym -> [Stmt sym]
                          I.NameVar _   -> error $ "XXX need to implement function pointers."
    I.Loop m _ e _ blk -> [Loop (Just (loopIdx m e)) (concatMap toCall blk)]
    _                  -> []

loopIdx :: Integer -> I.Expr -> Integer
loopIdx _ (I.ExpLit (I.LitInteger i)) = i
loopIdx m _                           = m

_getIdx :: I.Expr -> Maybe Integer
_getIdx e = case e of
             I.ExpLit (I.LitInteger i) -> Just i
             _                         -> Nothing

--------------------------------------------------------------------------------
-- Call-graph construction from a module.

-- type Node = G.LNode ProcInfo

-- | A call graph is a graph in which nodes are labeled by their procedure info
-- and edges are unlabeled.
type CFG = G.Gr ProcInfo ()

flattenControl :: Control -> [CallNm]
flattenControl ctrl =
  case ctrl of
    Stmt str           -> [str]
    Branch ctrl0 ctrl1 ->
      concatMap flattenControl ctrl0 ++ concatMap flattenControl ctrl1
    Loop _ ctrl0       -> concatMap flattenControl ctrl0

cfgModule :: I.Module -> ModuleInfo
cfgModule m = ModuleInfo
  { modName = I.modName m
  , procs   = map cfgProc ps
  }
    where ps = I.public (I.modProcs m) ++ I.private (I.modProcs m)

-- | Construct a control-flow graph from an Ivory module.
cfg :: I.Module -> CFG
cfg m = G.insEdges (concatMap go nodes) $ G.insNodes nodes G.empty
  where
  nodes :: [G.LNode ProcInfo]
  nodes = zip [0,1..] (procs $ cfgModule m)

  go :: (Int, ProcInfo) -> [G.LEdge ()]
  go (i,p) =
    let outCalls = concatMap flattenControl (calls p) in
    let outIdxs  = catMaybes (map (lookup nodes) outCalls) in
    zip3 (repeat i) outIdxs (repeat ()) -- outboud edges

  lookup ls sym | [] <- ls         = Nothing
                | ((i,p):_) <- ls
                , procSym p == sym = Just i
                | (_:ls') <- ls    = lookup ls' sym
                | otherwise        = error "Unreachable in cfg" -- To make GHC happy.

-- | Just label the nodes with the function names.
procSymGraph :: CFG -> G.Gr CallNm ()
procSymGraph = G.nmap procSym

-- | Does the program have a loop in it?
hasLoop :: CFG -> Bool
hasLoop = G.hasLoop

--------------------------------------------------------------------------------
-- Stack usage analysis.

data SizeMap = SizeMap
  { stackElemMap :: StackType -> Size -- ^ Mapping from `StackType` to their
                                      --   implementation-dependent size.
  , retSize      :: Size              -- ^ Size of a return address.
  }


-- | Maps everything to being size 1.  Useful for testing.
defaultSizeMap :: SizeMap
defaultSizeMap = SizeMap
  { stackElemMap = const 1
  , retSize      = 1
  }

-- A mapping from `Node` (Ints) to the maximum stack usage for that function.
type MaxMap = M.IntMap Size

newtype MaxState a = MaxState
  { unSt :: StateT MaxMap Id a
  } deriving (Functor, Monad, Applicative)

instance StateM MaxState MaxMap where
  get = derive_get (Iso MaxState unSt)
  set = derive_set (Iso MaxState unSt)

emptyMaxSt :: MaxMap
emptyMaxSt = M.empty

getMaxMap :: MaxState MaxMap
getMaxMap = return =<< get

-- | Takes a procedure name, a control-flow graph, and a `SizeMap` and produces
-- the maximum stack usage starting at the procedure give.  Returns `Top` if
-- there is an unanalyzable loop in the program (ie., non-constant loop) and
-- @Val max@ otherwise.
maxStack :: CallNm -> CFG -> SizeMap -> WithTop Size
maxStack proc cf szmap = go proc
  where
  go p = fst $ runM (unSt (maxStack' cf szmap [] (findNode cf p))) emptyMaxSt

-- Get the node from it's name.
findNode :: CFG -> CallNm -> G.Node
findNode cf proc = fst $
  fromMaybe (error $ "Proc " ++ proc ++ " is not in the graph!")
            (find ((== proc) . procSym . snd) (G.labNodes cf))

maxStack' :: CFG -> SizeMap -> [G.Node] -> G.Node -> MaxState (WithTop Size)
maxStack' cf szmap visited curr
  | curr `elem` visited = return Top -- A loop is detected.
  | otherwise           = maxStackNode
  where
  -- Process the max stack for a single node.
  maxStackNode :: MaxState (WithTop Size)
  maxStackNode = do
    blkMax <- goBlks cf szmap visited curr alloc' calls'
    let sz = (topAllocSz + paramsSz + retSize szmap +) <$> blkMax
    return sz

    where
    cxt = G.context cf curr
    procInfo = G.lab' cxt

    alloc' = alloc procInfo
    calls' = calls procInfo

    -- Top-level allocation
    topAllocSz :: Size
    topAllocSz = getSize szmap (getBlock alloc')

    paramsSz :: Size
    paramsSz = getSize szmap (params procInfo)

goBlks :: CFG -> SizeMap -> [G.Node] -> G.Node
       -> [Alloc] -> [Control] -> MaxState (WithTop Size)
goBlks cf szmap visited curr acs cns =
  case (acs, cns) of
    -- Done with all blocks/statements in this function.
    ([] ,[])                               -> return (Val 0)
    (Branch a0 a1:acs', Branch c0 c1:cns') -> do
      sz0 <- goBlks' a0 c0
      sz1 <- goBlks' a1 c1
      sz2 <- goBlks' acs' cns'
      return (liftA2 max sz0 sz1 <+> sz2)
    -- There's no new loop allocation, just assignments.  So we assume that on
    -- each iteration, the stack usage doesn't change.
    (Loop _ a:acs', Loop _ c:cns')         -> do
      sz0 <- goBlks' a c
      sz1 <- goBlks' acs' cns'
      return (sz0 <+> sz1)

      -- case idx of
      --   -- Unknown loop bound.
      --   Nothing -> if sz0 == Val 0 then return sz1

      --                else return Top -- Did some allocation in the loop, so
      --                                -- can't compute.
      --   Just _  ->
  -- There is either a straight-line call or assignment.
    _                                       -> do
      sz0 <- goBlk cf szmap visited curr (getBlock acs) (getBlock cns)
      sz1 <- goBlks' (nxtBlock acs) (nxtBlock cns)
      return (sz0 <+> sz1)
  where goBlks' = goBlks cf szmap visited curr

goBlk :: CFG -> SizeMap -> [G.Node] -> G.Node -> [StackType]
      -> [CallNm] -> MaxState (WithTop Size)
goBlk cf szmap visited curr acs cns = do
  maxMp <- getMaxMap
  let localAlloc = getSize szmap acs
  let callNodes  = map (findNode cf) cns
  let allCalls   = zip callNodes (map (flip M.lookup $ maxMp) callNodes)
  newMaxs <- mapM cachedCalls allCalls
  return $ Val localAlloc <+> if null newMaxs then Val 0
                                     -- Depends on Top being >= all vals.
                                else maximum newMaxs

  where
  cachedCalls :: (G.Node, Maybe Size) -> MaxState (WithTop Size)
  cachedCalls (n, msz) | Just sz <- msz = return (Val sz)
                       | otherwise      =
    maxStack' cf szmap (curr:visited) n

(<+>) :: Num a => WithTop a -> WithTop a -> WithTop a
(<+>) = liftA2 (+)

-- Get the cumulative sizes of allocated things.
getSize :: SizeMap -> [StackType] -> Size
getSize szmap = sum . map (stackElemMap szmap)

-- Get the prefix of a list of @[Block a] in the current block.
getBlock :: (Show a, Eq a) => [Block a] -> [a]
getBlock bls | (Stmt b:bs) <- bls = b : getBlock bs
             | otherwise          = []

nxtBlock :: (Show a, Eq a) => [Block a] -> [Block a]
nxtBlock bls | (Stmt _:bs) <- bls = nxtBlock bs
             | otherwise           = bls

-- | Call-graph output.  Takes a start function foo, a filepath, and emits the
-- graph named "foo.dot" in the filepath.
callGraphDot :: CallNm -> FilePath -> [I.Module] -> IO CFG
callGraphDot proc path mods =
  writeFile (path </> proc `addExtension` "dot") grOut >> return graph
  where
  m = mconcat mods
  grOut = graphviz filterG (I.modName m)
  filterG :: G.Gr CallNm ()
  filterG = let closure = G.reachable (findNode graph proc) graph in
            G.delNodes (G.nodes graph \\ closure) (procSymGraph graph)
  graph = cfg m

--------------------------------------------------------------------------------
-- Adapted from fgl (BSD3) to remove sizing info.
graphviz :: (G.Graph g, Show a, Show b)
  => g a b   -- ^ The graph to format
  -> String  -- ^ The title of the graph
  -> String
graphviz g t =
    let n = G.labNodes g
        e = G.labEdges g
        ns = concatMap sn n
        es = concatMap se e
    in "digraph "++t++" {\n"
            ++ ns
            ++ es
        ++"}"
    where sn (n, a) | sa == ""  = ""
                    | otherwise = '\t':(show n ++ sa ++ "\n")
            where sa = sl a
          se (n1, n2, b) = '\t':(show n1 ++ " -> " ++ show n2 ++ sl b ++ "\n")

sl :: (Show a) => a -> String
sl a = let l = sq (show a)
       in if (l /= "()") then (" [label = \""++l++"\"]") else ""

sq :: String -> String
sq s@[_]                     = s
sq ('"':s)  | last s == '"'  = init s
            | otherwise      = s
sq ('\'':s) | last s == '\'' = init s
            | otherwise      = s
sq s                         = s

--------------------------------------------------------------------------------
-- Testing

{-

fibCFG :: CFG
fibCFG = cfg fibMod

outGraph = writeFile "fib.dot" gr
  where
  gr = G.graphviz (procSymGraph fibCFG) "fib" (4,4) (4,4) G.Portrait
----------------------

fibMod :: Module
fibMod = package "fib" $ do
  incl fib
  incl fib_aux

fib :: Def ('[Uint32] :-> Uint64)
fib  = proc "fib" (\n -> body (ret =<< call fib_aux 0 1 n))

fib_aux :: Def ('[Uint32,Uint32,Uint32] :-> Uint64)
fib_aux  = proc "fib_aux" $ \ a b n -> body $ do
  ifte_ (n ==? 0)
    (ret (safeCast a))
    (ret . safeCast =<< call fib_aux b (a + b) (n - 1))

fibSz = SizeMap
  { stackElemMap = \_ -> 1
  , retSize = 1 }

-------------------------------------

x :: G.Gr Int ()
x = G.insEdges edges gr
  where
  edges = [(0,1,()), (0,2,()), (2,1,()), (2,3,())]
  gr = G.insNodes nodes G.empty
  nodes = zip [0,1 ..] [2,3,4,1]

y :: G.Gr Int ()
y = G.insEdges edges gr
  where
  edges = [(0,1,()), (0,2,()), (2,1,()), (1,3,())]
  gr = G.insNodes nodes G.empty
  nodes = zip [0,1 ..] [1,4,5,8]

maxf :: G.Gr Int () -> G.Node -> Maybe Int
maxf g n = maxf' [] n
  where
  maxf' :: [G.Node] -> G.Node -> Maybe Int
  maxf' visited curr
    | curr `elem` visited = Nothing -- We made a loop on this path!  That's Top.
    | otherwise           = liftA2 (+) (Just $ G.lab' cxt) go
        where
        cxt = G.context g curr
        res = map (maxf' (G.node' cxt : visited)) (G.suc' cxt)
        go | null (G.suc' cxt)  = Just 0 -- No more successors to try
           | Nothing `elem` res = Nothing
           | otherwise          = Just $ maximum (catMaybes res)

-}
