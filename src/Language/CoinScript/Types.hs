{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Language.CoinScript.Types
    (
      Program
    , StateE
    , Op(..)
    , Status(..)
    , Machine(..)

    , isFail
    ) where

import Control.Monad.State
import Control.Monad.Error
import qualified Data.Text as T

-- custom State monad for handling failure
type StateE a = ErrorT String (State a)

-- language operations
data Op = OpNoop
        | OpInt Integer
        | OpAdd
        | OpDup
        | OpDrop
        | OpTrue | OpFalse
        | OpString T.Text
        | OpEmptyList
        | OpAppendList
        | OpCode Program
        | OpExecute
    deriving (Show)

type Program = [Op]

data Status = Ok
            | Fail String
    deriving (Show)

isFail :: Status -> Bool
isFail (Fail _) = True
isFail _ = False

-- describes functions applicable to all language machines
class Machine a b | a -> b where
    empty :: a
    load :: a -> Program -> a
    status :: a -> Status
    isDone :: a -> Bool
    step :: a -> a

{- TODO ideas for other interesting machines
 -
 - * dynamically typed machine (converts to/from types as needed)
 - * string evaluation machine (runs via pattern matching, like Joy can)
 -
 - I'm starting to think that more of the computation should move into
 - the machine itself.  One should be able to creat an empty machine,
 - load it with code, step it forward and ask if it's done.  All this
 - is done in the StateE monad so errors are handled nicely.  Everything
 - else is an implementation detail.
 -
 - Unfortunately, this design means that each machine has to implement
 - the operations for itself.  If there's a way to leave 'runOp' generic
 - like it is now, that'd be fantastic.  It might work for everything
 - besides the string evaluation machine.  That machine doesn't even
 - have a stack.  It just has an instruction tape (the string) that's
 - modified by repeated pattern matching.
 -}
