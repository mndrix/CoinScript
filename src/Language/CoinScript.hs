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
data Machine = Machine
    { stack :: [Item]
    } deriving (Show)
push :: Item -> State Machine ()
push i = do
    (Machine s) <- get
    put $ Machine (i:s)
pop :: State Machine Item
pop = do
    (Machine (x:s)) <- get
    put $ Machine s
    return x
peek :: State Machine Item
peek = do
    x <- pop
    push x
    return x
pushInteger :: Integer -> State Machine ()
pushInteger i = push (ItemInt i)
popInteger :: State Machine Integer
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

runOp :: Op -> State Machine ()
runOp OpNoop = return ()
runOp (OpInt i) = pushInteger i
runOp OpAdd = do
    x <- popInteger
    y <- popInteger
    pushInteger $ x+y
runOp OpDup = peek >>= push
runOp OpDrop = pop >> return ()

run :: Program -> Machine -> Machine
run p st = foldl (\s o -> execState (runOp o) s) st p
