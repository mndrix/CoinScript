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
data Stack = Stack
    { stack :: [Item]
    } deriving (Show)
push :: Item -> State Stack ()
push i = do
    (Stack s) <- get
    put $ Stack (i:s)
pop :: State Stack Item
pop = do
    (Stack (x:s)) <- get
    put $ Stack s
    return x
peek :: State Stack Item
peek = do
    x <- pop
    push x
    return x

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

runOp :: Op -> State Stack ()
runOp OpNoop = return ()
runOp (OpInt i) = push (ItemInt i)
runOp OpAdd = do
    (ItemInt x) <- pop
    (ItemInt y) <- pop
    push $ ItemInt (x+y)
runOp OpDup = peek >>= push
runOp OpDrop = pop >> return ()

run :: Program -> Stack -> Stack
run p st = foldl (\s o -> execState (runOp o) s) st p
