{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Eval where

import           Control.Monad.Reader
import qualified Data.HashMap.Strict           as HM
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as T
import           Primitives
import           Types

varLookup :: LispVal -> Eval LispVal
varLookup (Atom symbol) = asks
    (fromMaybe (error "Undefined") . reverseGet symbol)
  where
-- Top of stack has current scope.
    reverseGet :: Symbol -> [LispEnv] -> Maybe LispVal
    reverseGet symbol (x : xs) = case x HM.!? symbol of
        (Just v) -> return v
        Nothing  -> reverseGet symbol xs
    reverseGet symbol [] = Nothing

bindToPairM (List [Atom a, b]) = eval b >>= \res -> return (a, res)
bindToPairM _                  = error "Expected Atom-Expression Tuples"

-- First definining special forms
eval :: LispVal -> Eval LispVal
-- Auto Quote Literals
eval (Int    i ) = return $ Int i
eval (Float  i ) = return $ Float i
eval (String i ) = return $ String i
eval (Char   i ) = return $ Char i
eval (Bool   b ) = return $ Bool b
eval (List   []) = return Nil
eval Nil         = return Nil
-- Quoted Expressions
eval (List [Atom "quote", sexpr]) = return sexpr
-- Write Expressions
eval (List [Atom "write", arg]) = return $ String $ T.pack $ show arg
eval (List (Atom "write" : tail)) = return $ String $ T.pack $ show $ List tail
-- Lookup Atom
eval n@(Atom name) = varLookup n
-- Conditional : If Else
eval (List [Atom "if", test, isTrue, isFalse]) = eval test >>= \case
    (Bool True ) -> eval isTrue
    (Bool False) -> eval isFalse
    _            -> error "Incorrect special form if."
-- Conditional : cond
eval (List [Atom "cond", List []   ]) = error "Incorrect special form"
eval (List [Atom "cond", List pairs]) = check pairs
  where
    check :: [LispVal] -> Eval LispVal
    check ((List [predicate, consequent]) : xs) = do
        eval predicate >>= \case
            (Bool True ) -> eval consequent
            (Bool False) -> check xs
    check [] = return Nil
-- Assignment: Let
eval (List [Atom "let", List pairs, body]) = do
    k <- HM.fromList <$> mapM bindToPairM pairs
    local (k :) $ eval body
-- Assignment: Let*
eval (List [Atom "let*", List pairs, body]) = local (HM.empty :)
    $ func pairs body
  where
    func (x : xs) body = do
        (k, v) <- bindToPairM x
        nstate <- asks (HM.insert k v . head)
        local (\x -> nstate : tail x) $ func xs body
    func [] b = eval b
-- Sequencing: Begin
eval (List [Atom "begin", arg]                         ) = evalBody arg
eval (List ((:) (Atom "begin") rest)                   ) = evalBody $ List rest
--Sequencing: Do
eval (List [Atom "do", Atom v]) = undefined
-- Definitions
eval (List [Atom "define", a@(Atom variableExpr), expr]) = do
    env <- eval expr >>= \r -> asks (HM.insert variableExpr r . head)
    local (\s -> env : tail s) $ return a
-- Lambda Function
eval (List [Atom "lambda", List params, expr]) = do
    env <- asks head
    return $ Lambda (Func $ mkLambda expr params) env
-- Function Application
eval (List ((:) x xs)) = do
    function <- eval x
    args     <- mapM eval xs
    case function of
        (Fn (Func fn)             ) -> fn args
        (Lambda (Func fn) bindings) -> local (bindings :) $ fn args
        _                           -> error "Not a function"

mkLambda :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal
mkLambda expr params args = do
    params' <- mapM getParams params
    args'   <- mapM eval args
    local (\(x : xs) -> (HM.fromList (zip params' args') <> x) : xs) $ eval expr
  where
    getParams :: LispVal -> Eval Symbol
    getParams (Atom xs) = return xs
    getParams _         = error "Expected Atom"

evalBody :: LispVal -> Eval LispVal
evalBody (List [List [Atom "define", Atom name, res], next]) = do
    res'  <- evalBody res
    state <- asks (HM.insert name res' . head)
    local (\x -> state : tail x) $ evalBody next
evalBody (List (List [Atom "define", Atom name, res] : xs)) = do
    res'  <- evalBody res
    state <- asks (HM.insert name res' . head)
    local (\x -> state : tail x) $ evalBody $ List xs
evalBody (List [List (Atom "set!" : Atom var : xs), next]) = do
    nstate <- case xs of
        []  -> asks (HM.update (const Nothing) var . head)
        [x] -> asks (HM.update (const $ Just x) var . head)
        _   -> error "Bad Special Form: Set!"
    local (\x -> nstate : tail x) $ evalBody next
evalBody (List (List (Atom "set!" : Atom var : xs) : next)) = do
    nstate <- case xs of
        []  -> asks (HM.update (const Nothing) var . head)
        [x] -> asks (HM.update (const $ Just x) var . head)
        _   -> error "Bad Special Form: Set!"
    local (\x -> nstate : tail x) $ evalBody $ List next
evalBody x = eval x

runInEnv :: [LispEnv] -> Eval b -> IO b
runInEnv code (Eval action) = runReaderT action code

basicEnv :: [LispEnv]
basicEnv = [HM.fromList primitiveEnv]
