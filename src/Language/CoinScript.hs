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
type Stack = [Item]

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
runOp (OpInt i) = do
    s <- get
    put (ItemInt i:s)
runOp OpAdd = do
    (ItemInt x:ItemInt y:s) <- get
    put (ItemInt (x+y):s)
runOp OpDup = do
    (x:s) <- get
    put (x:x:s)
runOp OpDrop = do
    (_:s) <- get
    put s

run :: Program -> Stack -> Stack
run p st = foldl (\s o -> execState (runOp o) s) st p
