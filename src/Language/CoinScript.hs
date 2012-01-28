module Language.CoinScript
    (
    ) where

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

runOp :: Op -> Stack -> Stack
runOp OpNoop s = s
runOp (OpInt i) s = ItemInt i : s
runOp OpAdd (ItemInt x : ItemInt y : s) = ItemInt (x+y) : s
runOp OpAdd _ = error "+ requires two integers on the stack"
runOp OpDup s@(x:_) = x:s
runOp OpDup _ = error "d requires something on the stack"
runOp OpDrop (x:s) = s
runOp OpDrop _ = error "D requires something on the stack"

run :: Program -> Stack -> Stack
run p s = foldl (flip runOp) s p
