{-# LANGUAGE MultiParamTypeClasses #-}
module Language.CoinScript.DataMachine
    (
      DataMachine
    ) where

import Control.Monad.Error
import Control.Monad.State
import Language.CoinScript.Types
import qualified Data.Text as T

type Stack = [Item]

-- contents of a data stack
data Item = ItemInt Integer
          | ItemBool Bool
          | ItemString T.Text
          | ItemList Stack
          | ItemCode Program
    deriving (Show)

-- stack machine operating on data
data DataMachine = DataMachine
    { stack :: [Item]
    , code :: Program
    , dmStatus  :: Status
    } deriving (Show)
instance Machine DataMachine Item where
    empty    = DataMachine [] [] Ok
    load m p = m{code=p}
    status   = dmStatus
    isDone m = any ($m) [ null . code, isFail . status ]
    step   m = case runState (runErrorT nextOp) m of
                    (Right _,  n) -> n
                    (Left msg, n) -> n{dmStatus=Fail msg}

-- execute the machine's next op
nextOp :: StateE DataMachine ()
nextOp = do
    m <- get
    let op:ops = code m
    case op of
        OpExecute -> do
            let (ItemCode c:s) = stack m
            put m{stack=s,code=c++ops}
        _ -> case runOp op (stack m) of
                Left msg -> fail msg
                Right s -> put m{stack=s,code=ops}

runOp :: Op -> Stack -> Either String Stack
runOp  OpNoop         = Right . id
runOp  OpTrue         = Right . (ItemBool True:)
runOp  OpFalse        = Right . (ItemBool False:)
runOp  OpEmptyList    = Right . (ItemList []:)
runOp (OpInt       i) = Right . (ItemInt i:)
runOp (OpString    t) = Right . (ItemString t:)
runOp (OpCode      c) = Right . (ItemCode   c:)
runOp  OpExecute = \_ ->
    Left "OpExecute handled in nextOp only"
runOp  OpDrop = \s ->
    case s of
        [] -> Left "Can't drop from an empty stack"
        xs -> Right $ tail xs
runOp  OpDup = \s ->
    case s of
        [] -> Left "Can't dup an empty stack"
        xs -> Right $ head xs : xs
runOp  OpAdd = \s ->
    case s of
        ItemInt x:ItemInt y:zs -> Right $ ItemInt (x+y) : zs
        _ -> Left "OpAdd requires two integer arguments"
runOp  OpAppendList = \s ->
    case s of
        x:ItemList l:xs -> Right $ ItemList (l++[x]) : xs
        _ -> Left "OpAppendList requires an item and a list as arguments"
