module NCC.Internal (
    P.Bool(..),
    P.Int,
    P.Char,
    P.IO,
    P.putStrLn,
    P.getLine,
    P.error,
    fromInteger,
    ifThenElse,
    intEq,
    intIneq,
    intAdd,
    intSub,
    intMul,
    intNegate,
    intAbs,
    intShow,
    charEq,
    charIneq,
    charShow,
    returnIO,
    bindIO
) where

import qualified Prelude as P

fromInteger :: P.Integer -> P.Int
fromInteger = P.fromInteger

ifThenElse :: P.Bool -> a -> a -> a
ifThenElse P.True  t _ = t
ifThenElse P.False _ f = f

intEq :: P.Int -> P.Int -> P.Bool
intEq = (P.==)

intIneq :: P.Int -> P.Int -> P.Bool
intIneq = (P./=)

intAdd :: P.Int -> P.Int -> P.Int
intAdd = (P.+)

intSub :: P.Int -> P.Int -> P.Int
intSub = (P.-)

intMul :: P.Int -> P.Int -> P.Int
intMul = (P.*)

intNegate :: P.Int -> P.Int 
intNegate = P.negate

intAbs :: P.Int -> P.Int
intAbs = P.abs

intShow :: P.Int -> P.String
intShow = P.show

charEq :: P.Char -> P.Char -> P.Bool
charEq = (P.==)

charIneq :: P.Char -> P.Char -> P.Bool
charIneq = (P./=)

charShow :: P.Char -> P.String
charShow = P.show

returnIO :: a -> P.IO a
returnIO = P.return

bindIO :: P.IO a -> (a -> P.IO b) -> P.IO b
bindIO = (P.>>=)