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
    deriving (Show)

type Program = [Op]

-- contents of a data stack
data Item = ItemInt Integer
          | ItemBool Bool
          | ItemString T.Text
    deriving (Show)

-- contents of a type stack
data Type = TypeInt
          | TypeBool
          | TypeString
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

-- Parse script text into an executable program
parse :: String -> Program
parse str = reverse $ go str []
  where
    go []       p = p
    go (' ':cs) p = go cs (OpNoop:p)
    go ('+':cs) p = go cs (OpAdd:p)
    go ('d':cs) p = go cs (OpDup:p)
    go ('D':cs) p = go cs (OpDrop:p)
    go ('t':cs) p = go cs (OpTrue:p)
    go ('f':cs) p = go cs (OpFalse:p)
    go ('"':cs) p =
        let (chars,(_:rest)) = span (/='"') cs in
        go rest ((OpString $ T.pack chars):p)
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
runOp OpTrue = pushBoolean True
runOp OpFalse = pushBoolean False
runOp (OpString s) = pushString s

runProgram :: Machine a b => Program -> a -> a
runProgram p st = foldl (\s o -> execState (runOp o) s) st p

runScript :: Machine a b => String -> a -> a
runScript = runProgram . parse
