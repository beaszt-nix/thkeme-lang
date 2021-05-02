{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Syntax where

import           Data.Hashable
import           Data.Int                       ( Int64 )
import           Text.Printf

newtype LispDouble = LsD { getLsD :: Double } deriving (Eq, Show, Ord, Read, Enum, Num, Fractional, Floating, Real, RealFrac)
newtype LispInt = LsI { getLsI :: Int64 } deriving (Eq, Show, Read, Ord, Num, Real, Enum, Integral, Bounded)
newtype LispBool = LsB { getLsB :: Bool } deriving (Eq, Bounded, Read, Enum)
newtype LispChar = LsC { getLsC :: Char } deriving (Eq,Ord,Show,Read)
type Symbol = String

data LispExpr =
    Float LispDouble
  | Int LispInt
  | Bool LispBool
  | Char LispChar
  | String [LispChar]
  | Symbol Symbol
  | SExpr [LispExpr]
  | QExpr [LispExpr]

data LispErr = InsufficientArg String Int Int
             | Undefined String
             | TypeError String String String
             | GeneralError String

instance Show LispBool where
    show (LsB True ) = "#t "
    show (LsB False) = "#f "

instance Show LispExpr where
    show (Float  ldub ) = show ldub ++ " "
    show (Int    lint ) = show lint ++ " "
    show (Bool   lbool) = show lbool ++ " "
    show (Char   lchar) = show lchar ++ " "
    show (String lstr ) = show lstr ++ " "
    show (Symbol lsym ) = lsym ++ " "
    show (SExpr  lsexp) = concat ["(", concatMap show lsexp, ")"]
    show (QExpr  lsexp) = concat ["{", concatMap show lsexp, "}"]

instance Show LispErr where
    show (InsufficientArg name expected given) = printf
        "Function %s expected %d arguments, recieved %d"
        name
        expected
        given
    show (Undefined name) = printf "Object %s isn't defined"
    show (TypeError name expected recieved) =
        printf "%s expected to have type %s, got %s" name expected recieved
    show (GeneralError name) = "Error: " ++ name
