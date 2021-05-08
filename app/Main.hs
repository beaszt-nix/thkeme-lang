module Main where

import           Control.Monad.Trans.State      ( evalStateT )
import qualified Data.HashMap.Strict           as HM
import           Eval                           ( basicEnv )
import           Repline                        ( repl )


main :: IO ()
main = evalStateT repl (HM.empty : basicEnv)
