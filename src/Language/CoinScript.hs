{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Language.CoinScript
    (
    ) where

import Control.Monad.State
import Data.Char
import qualified Data.Map as M
import qualified Data.Text as T

data Op = OpNoop
        | OpInt Integer
        | OpAdd
        | OpDup
        | OpDrop
        | OpTrue  | OpFalse
        | OpString T.Text
        | OpEmptyList
        | OpAppendList
        | OpCode Program
        | OpExecute
    deriving (Show)

type Program = [Op]

-- contents of a data stack
data Item = ItemInt Integer
          | ItemBool Bool
          | ItemString T.Text
          | ItemList [Item]
          | ItemCode Program
    deriving (Show)

-- contents of a type stack
data Type = TypeInt
          | TypeBool
          | TypeString
          | TypeList [Type]
          | TypeCode [Type] [Type]
          | TypeVar Int
    deriving (Eq,Show)

-- describes functions applicable to all stack machines
class Machine a b | a -> b where
    load :: Program -> a
    stack :: a -> [b]
    setStack :: a -> [b] -> a
    codeStack :: a -> Program
    setCodeStack :: a -> Program -> a
    isDone :: a -> Bool
    isDone = null . codeStack
    push :: b -> State a ()
    push t = do
        m <- get
        put $ setStack m (t : stack m)
    pop :: State a b
    peek :: State a b
    peek = do
        x <- pop
        push x
        return x
    pushInteger :: Integer -> State a ()
    popInteger :: State a Integer
    pushBoolean :: Bool -> State a ()
    pushString :: T.Text -> State a ()
    pushList :: [b] -> State a ()
    popList :: State a [b]
    pushCode :: Program -> State a ()
    runCode :: State a ()
    postOp :: State a () -- hook for after op execution
    postOp = return ()

-- a stack machine for operating on data
data DataMachine = DataMachine
    { dmStack :: [Item]
    , dmCodeStack :: Program
    } deriving (Show)
instance Machine DataMachine Item where
    load p = DataMachine [] p
    stack = dmStack
    setStack m s = m{dmStack=s}
    codeStack = dmCodeStack
    setCodeStack m s = m{dmCodeStack=s}
    pop = do
        m <- get
        let (x:s) = stack m
        put $ setStack m s
        return x
    pushInteger = push . ItemInt
    popInteger = do
        (ItemInt i) <- pop
        return i
    pushBoolean = push . ItemBool
    pushString = push . ItemString
    pushList = push . ItemList
    popList = do
        (ItemList l) <- pop
        return l
    pushCode = push . ItemCode
    runCode = do
        m <- get
        let (ItemCode c:s) = stack m
        let p = codeStack m
        put $ setStack (setCodeStack m (c ++ p)) s

-- a stack machine for operating on types
data TypeMachine = TypeMachine
    { tmStack :: [Type]
    , typeQueue :: [Type]
    , tmCodeStack :: Program
    , tmNextTypeVar :: Int
    , tmResolvedTypes :: M.Map Int Type  -- how to resolve type variables
    } deriving (Show)
instance Machine TypeMachine Type where
    load p = TypeMachine [] [] p 1 M.empty
    stack = tmStack
    setStack m s = m{tmStack=s}
    codeStack = tmCodeStack
    setCodeStack m s = m{tmCodeStack=s}
    pop = do
        m <- get
        case stack m of
            [] -> do
                t <- nextTypeVar
                enqueue t
                return t
            (t:ts) -> do
                put $ setStack m ts
                return t
    pushInteger _ = push TypeInt
    popInteger = do
        m <- get
        case stack m of
            [] -> enqueue TypeInt
            (TypeInt:ts) -> put $ setStack m ts
            (TypeVar n:ts) -> do
                put $ setStack m ts
                resolveType n TypeInt
            (t:_) -> error $ "Expected TypeInt on stack, found " ++ show t
        return 1
    pushBoolean _ = push TypeBool
    pushString _ = push TypeString
    pushList = push . TypeList
    popList = do
        m <- get
        case stack m of
            [] -> do
                enqueue $ TypeList []
                return []
            (TypeList l:ts) -> do
                put $ setStack m ts
                return l
            (TypeVar n:ts) -> do
                put $ setStack m ts
                resolveType n (TypeList [])
                return []
            (t:_) -> error $ "Expected TypeList on stack, found " ++ show t
    pushCode p = do
        let (consume,produce) = inferType p
        push $ TypeCode consume produce
    runCode = do
        m <- get
        case stack m of
            [] -> enqueue $ TypeCode [] []
            (TypeCode c p:s) -> do
                put $ setStack m s
                consumeProduce c p
            (t:_) -> error $ "Expected TypeCode, found " ++ show t
    postOp = resolveTypes

-- helper for TypeMachine. Adds a value to the type queue
enqueue :: Type -> State TypeMachine ()
enqueue t = do
    m <- get
    put $ m{typeQueue=(t : typeQueue m)}

-- helper for TypeMachine's runCode implementation
-- consumes the first list of types from the stack and then produces
-- the second list of types onto the stack
consumeProduce :: [Type] -> [Type] -> State TypeMachine ()
consumeProduce [] [] = return ()
consumeProduce [] (p:ps) = push p >> consumeProduce [] ps
consumeProduce (c:cs) ps = do
    m <- get
    case stack m of
        [] -> do
            enqueue c
            consumeProduce cs ps
        (x:_) ->
            if c == x
                then pop >> consumeProduce cs ps
                else error $ "Expected " ++ show c ++ ", found " ++ show x

nextTypeVar :: State TypeMachine Type
nextTypeVar = do
    m <- get
    let n = tmNextTypeVar m
    put $ m{tmNextTypeVar=(n+1)}
    return $ TypeVar n

resolveType :: Int -> Type -> State TypeMachine ()
resolveType n t = do
    m <- get
    let rts = tmResolvedTypes m
    case M.lookup n rts  of
        Nothing -> put $ m{tmResolvedTypes=M.insert n t rts}
        Just t' -> when (t/=t') (error $ "Expected " ++ show t' ++ ", found " ++ show t)

resolveTypes :: State TypeMachine ()
resolveTypes = do
    m <- get
    let rts = tmResolvedTypes m
    when (not $ M.null rts) $ do
        let s = replaceTypes rts $ stack m
        let q = replaceTypes rts $ typeQueue m
        put $ (setStack m s){typeQueue=q,tmResolvedTypes=M.empty}

replaceTypes :: M.Map Int Type -> [Type] -> [Type]
replaceTypes _ [] = []
replaceTypes m (TypeVar n:xs) =
    case M.lookup n m of
        Nothing -> TypeVar n : replaceTypes m xs
        Just t  -> t : replaceTypes m xs
replaceTypes m (TypeList l:xs) = TypeList (replaceTypes m l) : replaceTypes m xs
replaceTypes m (x:xs) = x : replaceTypes m xs

-- Parse script text into an executable program
parse :: String -> Program
parse str = reverse $ fst $ go str [] 0
  where
    go :: String -> Program -> Integer -> (Program,String)
    go []       p 0 = (p,[])
    go []       _ _ = error "Unbalanced parentheses"
    go (' ':cs) p n = go cs (OpNoop:p) n
    go ('+':cs) p n = go cs (OpAdd:p) n
    go ('d':cs) p n = go cs (OpDup:p) n
    go ('D':cs) p n = go cs (OpDrop:p) n
    go ('t':cs) p n = go cs (OpTrue:p) n
    go ('f':cs) p n = go cs (OpFalse:p) n
    go ('!':cs) p n = go cs (OpExecute:p) n
    go ('(':cs) p n = go cs (OpEmptyList:p) (n+1)
    go (',':cs) p n = go cs (OpAppendList:p) n
    go (')':_ ) _ 0 = error "Closing paren without matching open paren"
    go (')':cs) p n = go cs (OpNoop:p) (n-1)
    go ('[':cs) p n =
        let (program,rest) = go cs [] 0 in
        go rest (OpCode (reverse program) : p) n
    go (']':cs) p 0 = (p,cs)
    go (']':_ ) _ _ = error "Unbalanced parentheses in code literal"
    go ('"':cs) p n =
        let (chars,(_:rest)) = span (/='"') cs in
        go rest ((OpString $ T.pack chars):p) n
    go s@(c:_)  p n
        | isDigit c =
            let (digits,rest) = span isDigit s in
            go rest ((OpInt $ read digits) : p) n
    go (_:cs) p n = go cs p n

runOp :: Machine a b => Op -> State a ()
runOp OpNoop = return ()
runOp (OpInt i) = pushInteger i
runOp OpAdd = do
    x <- popInteger
    y <- popInteger
    pushInteger $ x+y
runOp OpDup = peek >>= push
runOp OpDrop = pop >> return ()
runOp OpTrue = pushBoolean True
runOp OpFalse = pushBoolean False
runOp (OpString s) = pushString s
runOp OpEmptyList = pushList []
runOp OpAppendList = do
    x <- pop
    l <- popList
    pushList $ l ++ [x]
runOp (OpCode p) = pushCode p
runOp OpExecute = runCode

-- run the machine through its next operation
step :: Machine a b => a -> a
step = execState $ do
    m <- get
    let (op:program) = codeStack m
    put $ setCodeStack m program
    runOp op
    postOp

runProgram :: Machine a b => Program -> a
runProgram = until isDone step . load

runScript :: Machine a b => String -> a
runScript = runProgram . parse

inferType :: Program -> ([Type],[Type])
inferType p = ( reverse $ typeQueue machine, stack machine )
  where
    machine = runProgram p
