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

data Item = ItemInt Integer
    deriving (Show)

-- describes functions applicable to all stack machines
class Machine a where
    push :: Item -> State a ()
    pop :: State a Item
    peek :: State a Item
    peek = do
        x <- pop
        push x
        return x
    pushInteger :: Integer -> State a ()
    popInteger :: State a Integer

-- a stack machine for operating on data
data DataMachine = DataMachine
    { stack :: [Item]
    } deriving (Show)
instance Machine DataMachine where
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

runOp :: Machine a => Op -> State a ()
runOp OpNoop = return ()
runOp (OpInt i) = pushInteger i
runOp OpAdd = do
    x <- popInteger
    y <- popInteger
    pushInteger $ x+y
runOp OpDup = peek >>= push
runOp OpDrop = pop >> return ()

run :: Machine a => Program -> a -> a
run p st = foldl (\s o -> execState (runOp o) s) st p
