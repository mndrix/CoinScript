{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Language.CoinScript
    (
    ) where

import Control.Monad.State
import Data.Char

data Op = OpNoop
        | OpInt Integer
        | OpAdd
        | OpDup
        | OpDrop
    deriving (Show)

type Program = [Op]

-- contents of a data stack
data Item = ItemInt Integer
    deriving (Show)

-- contents of a type stack
data Type = TypeInt
          | TypeUnknown
    deriving (Show)

-- describes functions applicable to all stack machines
class Machine a b | a -> b where
    push :: b -> State a ()
    pop :: State a b
    peek :: State a b
    peek = do
        x <- pop
        push x
        return x
    pushInteger :: Integer -> State a ()
    popInteger :: State a Integer

-- a stack machine for operating on data
data DataMachine = DataMachine
    { itemStack :: [Item]
    } deriving (Show)
instance Machine DataMachine Item where
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

-- a stack machine for operating on types
data TypeMachine = TypeMachine
    { typeStack :: [Type]
    } deriving (Show)
instance Machine TypeMachine Type where
    push t = do
        (TypeMachine s) <- get
        put $ TypeMachine (t:s)
    pop = do
        (TypeMachine (t:s)) <- get
        put $ TypeMachine s
        return t
    pushInteger _ = push TypeInt
    popInteger = do
        TypeInt <- pop
        return 1

-- Parse script text into an executable program
parse :: String -> Program
parse str = reverse $ go str []
  where
    go []       p = p
    go (' ':cs) p = go cs (OpNoop:p)
    go ('+':cs) p = go cs (OpAdd:p)
    go ('d':cs) p = go cs (OpDup:p)
    go ('D':cs) p = go cs (OpDrop:p)
    go s@(c:_)  p
        | isDigit c =
            let (digits,rest) = span isDigit s in
            go rest ((OpInt $ read digits) : p)
    go (_:cs) p = go cs p

runOp :: Machine a b => Op -> State a ()
runOp OpNoop = return ()
runOp (OpInt i) = pushInteger i
runOp OpAdd = do
    x <- popInteger
    y <- popInteger
    pushInteger $ x+y
runOp OpDup = peek >>= push
runOp OpDrop = pop >> return ()

run :: Machine a b => Program -> a -> a
run p st = foldl (\s o -> execState (runOp o) s) st p
