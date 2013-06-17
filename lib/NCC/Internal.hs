module NCC.Internal (
    P.Int,
    P.Char,
    P.IO,
    P.putStrLn,
    P.error,
    boolCoerce,
    fromInteger,
    intEq,
    intIneq,
    intAdd,
    intSub,
    intMul,
    intNegate,
    intAbs,
    intShow,
    returnIO,
    bindIO
) where

import qualified Prelude as P

boolCoerce :: a -> a -> P.Bool -> a
boolCoerce t _ P.True  = t
boolCoerce _ f P.False = f

fromInteger :: P.Integer -> P.Int
fromInteger = P.fromInteger

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

returnIO :: a -> P.IO a
returnIO = P.return

bindIO :: P.IO a -> (a -> P.IO b) -> P.IO b
bindIO = (P.>>=)