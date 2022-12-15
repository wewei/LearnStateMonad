{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Lib
    ( Opr
    , Token
    , tokenize
    -- , calculate
    ) where

import Control.Monad.State.Lazy ( State, state, evalState, execState, get, put )
import Control.Monad.Loops ( iterateUntilM )
import Data.Char ( isDigit, digitToInt )

data Opr = Add | Sub | Mul | Div deriving (Show, Eq)

instance Ord Opr where
    Add <= _ = True
    Sub <= _ = True
    _ <= Mul = True
    _ <= Div = True
    _ <= _   = False

data Token
    = NumToken Int
    | OprToken Opr
    | LBracket
    | RBracket
    | Space
    deriving (Show, Eq)

newtype CalculatorError = CalculatorError String deriving (Show, Eq)

type TokenizerState = Either CalculatorError [Token]

readChar :: Char -> State TokenizerState ()
readChar c = state $ ((), ) . \case
    Left err -> Left err
    Right ts -> case c of
        '+' -> Right $ OprToken Add : ts
        '-' -> Right $ OprToken Sub : ts
        '*' -> Right $ OprToken Mul : ts
        '/' -> Right $ OprToken Div : ts
        '(' -> Right $ LBracket : ts
        ')' -> Right $ RBracket : ts
        ' ' -> Right $ Space : ts
        _   ->
            if isDigit c
            then let d = digitToInt c in case ts of
                (NumToken x:rs) -> Right $ NumToken (x * 10 + d) : rs
                _               -> Right $ NumToken d: ts
            else Left $ CalculatorError $ "Unexpected char '" <> [c] <> "'"

tokenize :: String -> TokenizerState
tokenize = foldl (flip $ execState . readChar) $ Right []

type CalculatorState = Either CalculatorError [Token]

call :: Opr -> Int -> Int -> Either CalculatorError Int
call Add x y = Right $ x + y
call Sub x y = Right $ x - y
call Mul x y = Right $ x * y
call Div _ 0 = Left $ CalculatorError "Divide by zero"
call Div x y = Right $ x `div` y


shift :: Token -> State CalculatorState ()
-- shift t = state $ ((), ) . \case
--     Left err -> Left err
--     Right ts -> Right $ t:ts
shift t = do
    st <- get
    put $ fmap (t:) st
    

reduce :: State CalculatorState ()
reduce = do
    st <- get
    put $ st >>= \case
        (RBracket:NumToken x:LBracket:rs)      -> Right $ NumToken x : rs
        (NumToken y:OprToken op:NumToken x:rs) -> do z <- call op x y; Right $ NumToken z : rs
        _                                      -> Left $ CalculatorError "Illegal reduction"

finalize :: State CalculatorState (Either CalculatorError Int)
finalize = do
    st <- get
    return $ st >>= \case
        [NumToken x] -> Right x
        _            -> Left $ CalculatorError "Non-terminated expression"


-- calculate :: TokenizerState -> CalculatorState
-- calculate (Left err) = Left err
-- calculate (Right ts) = case ts of

