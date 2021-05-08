{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Repl where

import           Control.Exception              ( SomeException(..)
                                                , fromException
                                                , throw
                                                , try
                                                )
import           Control.Monad.Reader
import qualified Data.HashMap.Strict           as HM
import           Data.Maybe
import qualified Data.Text                     as T
import           Eval
import           Parser
import           Types

getAtom :: LispVal -> Eval T.Text
getAtom x = eval x >>= \case
    (Atom name) -> return name
    n           -> throw $ Types.TypeError "Expected Atom" n

isMember :: T.Text -> [LispEnv] -> Bool
isMember name = foldr ((||) . HM.member name) False

updateState :: Bool -> T.Text -> [LispVal] -> Eval [LispEnv]
updateState isDefine name val = do
    res <- case val of
        [x] -> Just <$> eval x
        _   -> return Nothing
    member <- asks (isMember name)
    case (isDefine, member, res) of
        (True , True , _) -> throw $ PError "Already Defined Value"
        (False, False, _) -> throw $ PError "Undefined values can't be set"
        (True, False, Just val) ->
            asks (\x -> HM.insert name val (head x) : tail x)
        (False, True, res) ->
            asks (\x -> HM.alter (const res) name (head x) : tail x)
        _ -> throw $ PError "Developer Error"

evalRepl :: LispVal -> Eval (Either [LispEnv] LispVal)
evalRepl (List [Atom "define", namexp, val]) = do
    name <- getAtom namexp
    Left <$> updateState True name [val]
evalRepl (List (Atom "set!" : namexp : xs)) = do
    name <- getAtom namexp
    Left <$> updateState False name xs
evalRepl x = Right <$> eval x

safeEval :: IO a -> IO (Either String a)
safeEval m = do
    res <- try m
    case res of
        Left (except :: SomeException) -> case fromException except of
            Just (enclosed :: LispExcept) -> return $ Left (show enclosed)
            Nothing                       -> return $ Left (show except)
        Right val -> return $ Right val
