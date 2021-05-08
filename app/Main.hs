module Main where
import           System.Console.Haskeline

import           Control.Exception
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Either
import qualified Data.Text                     as T
import           Eval
import           Parser
import           Repl
import           Types



evalLoop :: [LispEnv] -> InputT IO ()
evalLoop env = do
    input <- getInputLine "thkeme:> "
    case input of
        Nothing      -> return ()
        Just "#q" -> return ()
        Just ""      -> evalLoop env
        Just input   -> do
            case replParser $ T.pack input of
                (Left  err) -> outputStrLn ("SyntaxError: " ++ show err) >> evalLoop env
                (Right res) -> do
                    val <- liftIO
                        (   either (Right . String . T.pack) id
                        <$> safeEval (runInEnv env $ evalRepl res)
                        )
                    case val of
                        Right (String str) -> outputStrLn (show str) >> evalLoop env
                        Right xs -> outputStrLn (show xs) >> evalLoop env
                        Left env -> evalLoop env

main :: IO ()
main = runInputT defaultSettings $ evalLoop basicEnv
