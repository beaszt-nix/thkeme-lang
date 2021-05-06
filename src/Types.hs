{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Types where

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.HashMap.Strict           as HM
import           Data.Hashable
import           Data.Int                       ( Int64 )
import qualified Data.Text                     as T
import           Data.Typeable


newtype LispDouble = LsD { getLsD :: Double } deriving (Eq, Show, Ord, Read, Enum, Num, Fractional, Floating, Real, RealFrac)
newtype LispInt = LsI { getLsI :: Int64 } deriving (Eq, Show, Read, Ord, Num, Real, Enum, Integral, Bounded)
newtype LispBool = LsB { getLsB :: Bool } deriving (Eq, Bounded, Read, Enum)
newtype LispChar = LsC { getLsC :: Char } deriving (Eq,Ord,Show,Read)

type Symbol = T.Text
type LispEnv = HM.HashMap Symbol LispVal
newtype Eval a = Eval (ReaderT [LispEnv] IO a)
    deriving (Functor, Applicative, Monad, MonadReader [LispEnv], MonadIO)
newtype Func = Func { fn :: [LispVal] -> Eval LispVal }

data LispVal
  = Atom T.Text
  | List [LispVal]
  | Float LispDouble
  | Int   LispInt
  | String T.Text
  | Char Char
  | Bool Bool
  | Nil
  | Fn Func
  | Lambda Func LispEnv
  deriving(Typeable)

data LispExcept = TypeError T.Text LispVal
    deriving Typeable

instance Eq LispVal where
    (Atom   x) == (Atom   y) = x == y
    (Float  x) == (Float  y) = x == y
    (Int    x) == (Int    y) = x == y
    (String x) == (String y) = x == y
    (Char   x) == (Char   y) = x == y
    (Bool   x) == (Bool   y) = x == y
    Nil        == Nil        = True
    (List  x)  == (List  y)  = x == y
    _          == _          = False

instance Show LispExcept where
    show (Types.TypeError msg val) = concat [show msg, ": ", show val]

instance Exception LispExcept

instance Show LispVal where
    show (Atom   val  ) = T.unpack val
    show (Float  d    ) = show d
    show (Int    i    ) = show i
    show (String s    ) = show s
    show (Char   c    ) = show c
    show (Bool   True ) = "#t"
    show (Bool   False) = "#f"
    show Nil            = "nil"
    show (Fn _      )   = "<internal_function>"
    show (Lambda _ _)   = "<lambda_function>"
    show (List xs   )   = "(" ++ show xs ++ ")"
