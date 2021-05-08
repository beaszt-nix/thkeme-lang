{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Primitives where
import           Control.Monad.Except
import           Data.Monoid                    ( All(..)
                                                , Any(..)
                                                )
import qualified Data.Text                     as T
import           Types
import Control.Exception

type PrimitiveList = [(T.Text, LispVal)]
type BinFunc = LispVal -> LispVal -> Eval LispVal
type UnFunc = LispVal -> Eval LispVal

mkFunc = Fn . Func

numCast :: LispVal -> LispVal -> (LispVal, LispVal)
numCast (Float f ) (Int   i ) = (Float f, Float $ fromIntegral i)
numCast (Int   f ) (Float i ) = (Float (fromIntegral f), Float i)
numCast (Int   i1) (Int   i2) = (Int i1, Int i2)
numCast (Float f1) (Float f2) = (Float f1, Float f2)
numCast a          b          = throw $ Types.TypeError "Expected Int or Float" a

arithOp :: (forall a . Num a => a -> a -> a) -> BinFunc
arithOp op a b = do
    let (a', b') = numCast a b
    case (a', b') of
        (Float f1, Float f2) -> return $ Float $ op f1 f2
        (Int   i1, Int i2  ) -> return $ Int $ op i1 i2

relOp :: (forall a . Ord a => a -> a -> Bool) -> BinFunc
relOp op a b = do
    let (a', b') = numCast a b
    case (a', b') of
        (Float f1, Float f2) -> return $ Bool $ op f1 f2
        (Int   i1, Int i2  ) -> return $ Bool $ op i1 i2

lispFold :: BinFunc -> LispVal -> [LispVal] -> Eval LispVal
lispFold func base x@[]     = throw $ ArgsCount 2 x
lispFold func base [a, b] = func a b
lispFold func base list   = foldM func base list

binFunc :: BinFunc -> [LispVal] -> Eval LispVal
binFunc func [a, b] = func a b
binFunc _    x      = throw $ ArgsCount 2 x

unary :: UnFunc -> [LispVal] -> Eval LispVal
unary op [a] = op a
unary _  x   = throw $ ArgsCount 1 x

car :: [LispVal] -> Eval LispVal
car [List []     ] = return Nil
car [List (x : _)] = return x
car []             = return Nil
car x              = throw $ Types.TypeError "CAR expects List" (List x)

cdr :: [LispVal] -> Eval LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [List []      ] = return Nil
cdr []              = return Nil
cdr x               = throw $ Types.TypeError "CDR expects List" (List x)

cons :: [LispVal] -> Eval LispVal
cons [l, r@(List rs)] = return $ List (l : rs)
cons [l]              = return $ List [l]
cons []               = return $ List []
cons x                = throw $ Types.TypeError "CONS expects List" (List x)

eq x y = return $ Bool $ x == y

isAtom :: LispVal -> Eval LispVal
isAtom (Atom _) = return $ Bool True
isAtom _        = return $ Bool False

and :: [LispVal] -> Eval LispVal
and = return . Bool . getAll . foldMap (\(Bool b) -> All b)

any :: [LispVal] -> Eval LispVal
any = return . Bool . getAny . foldMap (\(Bool b) -> Any b)

list :: [LispVal] -> Eval LispVal
list x = return $ List x

primitiveEnv :: PrimitiveList
primitiveEnv =
    [ ("+"    , mkFunc $ lispFold (arithOp (+)) (Int 0))
    , ("*"    , mkFunc $ lispFold (arithOp (*)) (Int 1))
    , ("-"    , mkFunc $ binFunc (arithOp (-)))
    , ("<"    , mkFunc $ binFunc (relOp (<)))
    , (">"    , mkFunc $ binFunc (relOp (>)))
    , ("<="   , mkFunc $ binFunc (relOp (<=)))
    , (">="   , mkFunc $ binFunc (relOp (>=)))
    , ("=="   , mkFunc $ binFunc (relOp (==)))
    , ("/="   , mkFunc $ binFunc (relOp (/=)))
    , ("car"  , mkFunc car)
    , ("cdr"  , mkFunc cdr)
    , ("cons" , mkFunc cons)
    , ("eq?"  , mkFunc $ binFunc eq)
    , ("atom?", mkFunc $ unary isAtom)
    , ("and"  , mkFunc Primitives.and)
    , ("or"   , mkFunc Primitives.any)
    , ("list" , mkFunc list)
    ]
