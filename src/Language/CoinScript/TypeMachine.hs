{-# LANGUAGE MultiParamTypeClasses #-}
module Language.CoinScript.TypeMachine
    (
      TypeMachine
    ) where

import Control.Monad.Error
import Control.Monad.State
import Language.CoinScript.Types
import qualified Data.Map as M

type Stack = [Type]

-- contents of a type stack
data Type = TypeInt
          | TypeBool
          | TypeString
          | TypeList Type
          | TypeCode [Type] [Type]
          | TypeVar Int
    deriving (Eq,Show)

-- a stack machine for operating on types
data TypeMachine = TypeMachine
    { stack :: Stack
    , queue :: Stack
    , code :: Program
    , tmNextTypeVar :: Int
    , resolvedVars :: M.Map Int Type  -- how type variables have been resolved
    , tmStatus :: Status
    } deriving (Show)
instance Machine TypeMachine Type where
    empty = TypeMachine [] [] [] 1 M.empty Ok
    load m p = m{code=p}
    status = tmStatus
    isDone m = any ($m) [ null . code, isFail . status ]
    step   m = case runState (runErrorT nextOp) m of
                    (Right _,  n) -> n
                    (Left msg, n) -> n{tmStatus=Fail msg}

-- execute the machine's next op
nextOp :: StateE TypeMachine ()
nextOp = do
    m <- get
    let op:ops = code m
    put m{code=ops}
    runOp op
    resolveTypes

runOp :: Op -> StateE TypeMachine ()
runOp  OpNoop         = return ()
runOp  OpTrue         = push TypeBool
runOp  OpFalse        = push TypeBool
runOp (OpInt       _) = push TypeInt
runOp (OpString    _) = push TypeString
runOp (OpCode      p) =
    let (consumes,produces) = inferType p in
    push $ TypeCode consumes produces
runOp OpExecute = do
    mx <- pop
    case mx of
        Nothing -> enqueue (TypeCode [] [])
        Just (TypeCode c p) -> consumeProduce c p
        Just (TypeVar n) -> resolveVar n (TypeCode [] [])
        Just x -> expected (TypeCode [] []) x
runOp OpDrop = do
    t <- nextTypeVar
    consumeProduce [t] []
runOp OpDup = do
    t <- nextTypeVar
    consumeProduce [t] [t,t]
runOp OpAdd = consumeProduce [TypeInt,TypeInt] [TypeInt]
runOp OpEmptyList = do
    t <- nextTypeVar
    consumeProduce [] [TypeList t]
runOp OpAppendList = do
    t <- nextTypeVar
    consumeProduce [t,TypeList t] [TypeList t]

-- Adds an item to the stack
push :: Type -> StateE TypeMachine ()
push x = do
    m <- get
    put m{stack=x:stack m}

-- Removes top item from the stack and returns it
pop :: StateE TypeMachine (Maybe Type)
pop = do
    m <- get
    case stack m of
        [] -> return Nothing
        x:xs -> put m{stack=xs} >> return (Just x)

-- Adds a value to the type queue
enqueue :: Type -> StateE TypeMachine ()
enqueue t = do
    m <- get
    put m{queue=t:queue m}

-- helper for TypeMachine's runCode implementation
-- consumes the first list of types from the stack and then produces
-- the second list of types onto the stack
consumeProduce :: [Type] -> [Type] -> StateE TypeMachine ()
consumeProduce [] [] = return ()
consumeProduce [] (p:ps) = push p >> consumeProduce [] ps
consumeProduce (c:cs) ps = do
    mx <- pop
    maybe (enqueue c) (unify c) mx
    consumeProduce cs ps

-- Unify x with y, resolving type variables as necessary
unify :: Type -> Type -> StateE TypeMachine ()
unify TypeInt TypeInt = return ()
unify TypeBool TypeBool = return ()
unify TypeString TypeString = return ()
unify (TypeList x) (TypeList y) = unify x y
unify (TypeCode c0 p0) (TypeCode c1 p1) = do
    when (length c0 == length c1) (fail "Code fails unify on consumption")
    when (length p0 == length p1) (fail "Code fails unify on production")
    mapM_ (uncurry unify) (zip c0 c1)
    mapM_ (uncurry unify) (zip p0 p1)
unify (TypeVar m) (TypeVar n)
    | m == n    = return ()
    | otherwise = resolveVar m (TypeVar n)
unify (TypeVar m) t = resolveVar m t
unify t (TypeVar m) = resolveVar m t
unify x y = fail $ "Cannot unify " ++ show x ++ " with " ++ show y

nextTypeVar :: StateE TypeMachine Type
nextTypeVar = do
    m <- get
    let n = tmNextTypeVar m
    put $ m{tmNextTypeVar=(n+1)}
    return $ TypeVar n

resolveVar :: Int -> Type -> StateE TypeMachine ()
resolveVar n t = do
    m <- get
    let rts = resolvedVars m
    case M.lookup n rts  of
        Nothing -> put m{resolvedVars=M.insert n t rts}
        Just t' -> unify t' t

resolveTypes :: StateE TypeMachine ()
resolveTypes = do
    m <- get
    let rts = resolvedVars m
    when (not $ M.null rts) $ do
        let s    =   map (replaceVar rts) $ stack m
        let q    =   map (replaceVar rts) $ queue m
        let rts' = M.map (replaceVar rts)   rts
        if rts' == rts
            then put $ m{stack=s,queue=q,resolvedVars=M.empty}
            else do
                put $ m{stack=s,queue=q,resolvedVars=rts'}
                resolveTypes

replaceVar :: M.Map Int Type -> Type -> Type
replaceVar m (TypeVar n) = maybe (TypeVar n) id (M.lookup n m)
replaceVar m (TypeList t) = TypeList (replaceVar m t)
replaceVar m (TypeCode c p) =
    let f = replaceVar m in
    TypeCode (map f c) (map f p)
replaceVar _ x = x

expected :: (Show b, Machine a b) => b -> b -> StateE a ()
expected e f = fail $ "Expected " ++ show e ++ ", found " ++ show f

inferType :: Program -> ([Type],[Type])
inferType p = ( reverse $ queue machine, stack machine )
  where
    machine = until isDone step $ load empty p
