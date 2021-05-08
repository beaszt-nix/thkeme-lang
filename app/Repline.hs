{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Repline where

import           Control.Monad.State
import           Data.Either
import qualified Data.Text                     as T
import           Eval
import           Parser
import           Repl
import           System.Console.Repline
import           Types

type Repl a = HaskelineT (StateT [LispEnv] IO) a

cmdRun :: LispVal -> Repl ()
cmdRun val = do
            env <- get
            val <- liftIO
                (   either (Right . String . T.pack) id
                <$> safeEval (runInEnv env $ replEval val)
                )
            either put (liftIO . print) val


cmd :: String -> Repl ()
cmd input = do
    case replParser $ T.pack input of
        (Left  err) -> liftIO $ print ("SyntaxError: " ++ show err)
        (Right val) -> cmdRun val

opts = [("quit", const abort), ("import", importCmd)]

importCmd :: FilePath -> Repl ()
importCmd fp = do
  res <- liftIO $ parseFile fp
  case res of
    (Left err) -> liftIO $ print ("SyntaxError: " ++ show err)
    (Right val) -> do
        env <- get
        mapM_ cmdRun val

replInit :: Repl ()
replInit =
    liftIO $ putStrLn "Welcome to the Thkeme Interpreter\n\t Scheme with a lisp"

fin = return Exit

thkemeBanner = \case
    MultiLine  -> return ">\t"
    SingleLine -> return "thkeme:> "

defaultOps :: ReplOpts (StateT [LispEnv] IO)
defaultOps = ReplOpts { banner           = thkemeBanner
                      , command          = cmd
                      , options          = opts
                      , prefix           = Just ':'
                      , multilineCommand = Just "multi"
                      , tabComplete      = Custom $ listCompleter []
                      , initialiser      = replInit
                      , finaliser        = fin
                      }

repl = dontCrash $ evalReplOpts defaultOps
