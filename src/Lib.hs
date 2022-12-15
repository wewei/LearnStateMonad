{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Lib
    ( Opr
    , Token
    , tokenize
    , calculate
    ) where

import Control.Monad.State.Lazy ( State, state, execState )
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
tokenize = fmap reverse . foldl (flip $ execState . readChar) (Right [])

type CalculatorState = Either CalculatorError ([Token], [Token])

call :: Opr -> Int -> Int -> Either CalculatorError Int
call Add x y = Right $ x + y
call Sub x y = Right $ x - y
call Mul x y = Right $ x * y
call Div _ 0 = Left $ CalculatorError "Divide by zero"
call Div x y = Right $ x `div` y


shift :: CalculatorState -> CalculatorState
shift = (=<<) $ \case
    (st, t:ts) -> Right (t:st, ts)
    _          -> Left (CalculatorError "Illegal shift")

skip :: CalculatorState -> CalculatorState
skip = (=<<) $ \case
    (st, _:ts) -> Right (st, ts)
    _          -> Left (CalculatorError "Illegal drop")

reduce :: CalculatorState -> CalculatorState
reduce = (=<<) $ \case
    (RBracket:NumToken x:LBracket:rs, ts)      -> Right (NumToken x : rs, ts)
    (NumToken y:OprToken op:NumToken x:rs, ts) -> do z <- call op x y; Right (NumToken z : rs, ts)
    stat                                       -> Left $ CalculatorError $ "Illegal reduction " <> show stat

calculate :: TokenizerState -> Either CalculatorError Int
calculate = fmap finalize . (=<<) (until terminate next . Right . ([],)) where
    terminate (Left _)                   = True
    terminate (Right ([NumToken _], [])) = True
    terminate _                          = False

    next :: CalculatorState -> CalculatorState
    next stat = stat >>= ($ stat) . \(st, ts) -> case ts of
        []            -> reduce
        NumToken _:_  -> shift
        LBracket:_    -> shift
        RBracket:_    -> case st of
            _:(OprToken _):_ -> reduce
            _                -> reduce . shift
        Space:_       -> skip
        OprToken op:_ -> case st of
            _:(OprToken opT):_ -> if op > opT then shift else reduce
            _                  -> shift

    finalize ([NumToken x], []) = x
    finalize _                  = 0