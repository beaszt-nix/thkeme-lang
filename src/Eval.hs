{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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
eval (List [Atom "let", List pairs, body]) =
    let k = HM.fromList $ map func pairs in local (k :) $ eval body
  where
    func (List [Atom a, b]) = (a, b)
    func _                  = error "Expected Atom-Expression Tuples"
-- Sequencing: Begin
eval (List [Atom "begin", arg]                         ) = evalBody arg
eval (List ((:) (Atom "begin") rest)                   ) = evalBody $ List rest
-- Defininitions
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
  res' <- evalBody res
  state <- asks (HM.insert name res' . head)
  local (\x -> state: tail x) $ evalBody next
evalBody (List (List [Atom "define" , Atom name , res] : xs)) = do
    res'  <- evalBody res
    state <- asks (HM.insert name res' . head )
    local (\x -> state : tail x) $ evalBody $ List xs
evalBody x = eval x

runInEnv :: [LispEnv] -> Eval b -> IO b
runInEnv code (Eval action) = runReaderT action code

basicEnv :: [LispEnv]
basicEnv = [HM.fromList primitiveEnv]
