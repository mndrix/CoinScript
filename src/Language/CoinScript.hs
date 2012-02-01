{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Language.CoinScript
    (
    ) where

import Control.Monad.State
import Data.Char
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
    deriving (Show)

type Program = [Op]

-- contents of a data stack
data Item = ItemInt Integer
          | ItemBool Bool
          | ItemString T.Text
          | ItemList [Item]
    deriving (Show)

-- contents of a type stack
data Type = TypeInt
          | TypeBool
          | TypeString
          | TypeList [Type]
          | TypeUnknown
    deriving (Show)

-- describes functions applicable to all stack machines
class Machine a b | a -> b where
    empty :: a
    push :: b -> State a ()
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

-- a stack machine for operating on data
data DataMachine = DataMachine
    { itemStack :: [Item]
    } deriving (Show)
instance Machine DataMachine Item where
    empty = DataMachine []
    push i = do
        (DataMachine s) <- get
        put $ DataMachine (i:s)
    pop = do
        (DataMachine (x:s)) <- get
        put $ DataMachine s
        return x
    pushInteger i = push (ItemInt i)
    popInteger = do
        (ItemInt i) <- pop
        return i
    pushBoolean = push . ItemBool
    pushString = push . ItemString
    pushList = push . ItemList
    popList = do
        (ItemList l) <- pop
        return l

-- a stack machine for operating on types
data TypeMachine = TypeMachine
    { typeStack :: [Type]
    , typeQueue :: [Type]
    } deriving (Show)
instance Machine TypeMachine Type where
    empty = TypeMachine [] []
    push t = do
        (TypeMachine s q) <- get
        put $ TypeMachine (t:s) q
    pop = do
        (TypeMachine s q) <- get
        case s of
            [] -> do
                put $ TypeMachine s (TypeUnknown:q)
                return TypeUnknown
            (t:ts) -> do
                put $ TypeMachine ts q
                return t
    pushInteger _ = push TypeInt
    popInteger = do
        (TypeMachine s q) <- get
        case s of
            [] -> put $ TypeMachine s (TypeInt:q)
            (TypeInt:ts) -> put $ TypeMachine ts q
            (t:_) -> error $ "Expected TypeInt on stack, found " ++ show t
        return 1
    pushBoolean _ = push TypeBool
    pushString _ = push TypeString
    pushList = push . TypeList
    popList = do
        (TypeMachine s q) <- get
        case s of
            [] -> do
                put $ TypeMachine s (TypeList []:q)
                return []
            (TypeList l:ts) -> do
                put $ TypeMachine ts q
                return l
            (t:_) -> error $ "Expected TypeList on stack, found " ++ show t

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
    go ('(':cs) p n = go cs (OpEmptyList:p) (n+1)
    go (',':cs) p n = go cs (OpAppendList:p) n
    go (')':_ ) _ 0 = error "Closing paren without matching open paren"
    go (')':cs) p n = go cs (OpNoop:p) (n-1)
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

runProgram :: Machine a b => Program -> a -> a
runProgram p st = foldl (\s o -> execState (runOp o) s) st p

runScript :: Machine a b => String -> a -> a
runScript = runProgram . parse
