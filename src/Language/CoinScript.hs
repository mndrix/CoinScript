module Language.CoinScript
    (
    ) where

import Data.Char
import Language.CoinScript.Types
import Language.CoinScript.DataMachine
import Language.CoinScript.TypeMachine
import qualified Data.Text as T

-- Parse script text into a program
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

runMachine :: Machine a b => a -> a
runMachine = until isDone step

runProgram :: Machine a b => Program -> a
runProgram = runMachine . load empty

runScript :: Machine a b => String -> a
runScript = runProgram . parse
