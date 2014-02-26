{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module Cada.Location (
>   Pos(..),
>   Positioned(..),
>   Loc(..),
>   inAutoPos,
>   RecA(..),
>   LocP(..),
>   inL,
>   inP,
>   returnL,
>   unL,
>   unLs,
>   getL,
>   transL,
>   headL,
>   listL,
>   tokenL,
>   ppPosn,
>   ppLoc,
>   ppLocDefs,
>   ppLocStr
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Cada.Lexer
>   import Cada.Pos

>   import Utility.PrettyPrint

    {----------------------------------------------------------------------}
    {-- Type Classes                                                      -}
    {----------------------------------------------------------------------}

>   class Positioned a where
>       srcPos :: a -> Pos

    {----------------------------------------------------------------------}
    {-- Locations                                                         -}
    {----------------------------------------------------------------------}
 
    Adds location information to an arbitrary type.

>   data Loc a = Loc {
>       locValue :: a,
>       locPosn  :: Pos
>   }

>   inAutoPos :: a -> Loc a
>   inAutoPos x = Loc x AutoGenPos

>   instance Functor Loc where
>       fmap f (Loc a p) = Loc (f a) p

>   instance Positioned (Loc a) where
>       srcPos = locPosn

    Adds location information to an arbitrary type and additionally
    applies it to the location type.

>   type RecA f a = f (a f)
>   type LocP a   = RecA Loc a

    Adds location information to a value.

>   inL :: AlexPosn -> a -> Loc a
>   inL = flip Loc . FilePos

>   inP :: Pos -> a -> Loc a
>   inP = flip Loc

    Monadic inL (for convenience).

>   returnL :: (Monad m) => AlexPosn -> a -> m (Loc a)
>   returnL p = return . inL p

    Removes location information from a value.

>   unL :: Loc a -> a
>   unL (Loc v _) = v

>   unLs :: [Loc a] -> [a]
>   unLs = map unL

    Extracts location information from a value.

>   getL :: Loc a -> Pos
>   getL (Loc _ p) = p

    Transfers location information from one value to another.

>   transL :: Loc a -> b -> Loc b
>   transL (Loc _ p) v = Loc v p 

    Obtains a location from the head of a list of values which
    have location information attached to them.

>   headL :: [Loc a] -> Pos
>   headL = getL . head

    Converts a list of values with locations to a list with the
    location of the first element (assumption: list has at least
    one element).

>   listL :: [Loc a] -> Loc [a]
>   listL xs = Loc (map unL xs) (headL xs)

    Extracts the string value of an identifier token and wraps
    it into a location.

>   tokenL :: TokenP -> Loc String
>   tokenL (p,t) = Loc (tVal t) (FilePos p)

    {----------------------------------------------------------------------}
    {-- Pretty Printing                                                   -}
    {----------------------------------------------------------------------}
    
>   ppPosn :: Pos -> ShowS
>   ppPosn UnknownPos =
>       showString "<unknown>"
>   ppPosn AutoGenPos =
>       showString "<generated>"
>   ppPosn (FilePos (AlexPn _ l c)) = 
>       showString "<line " .
>       showString (show l) .
>       showString ", column " .
>       showString (show c) .
>       showString ">" 

>   ppLoc :: Loc a -> ShowS
>   ppLoc (Loc _ p) = ppPosn p
    
>   ppLocDefs :: (a -> ShowS) -> [Loc a] -> ShowS
>   ppLocDefs f []             = id
>   ppLocDefs f (Loc x p : xs) = ppPosn p . f x . ppLocDefs f xs

>   ppLocStr :: FilePath -> Loc String -> String
>   ppLocStr fp (Loc xs p) = ppDo $
>       showString fp .
>       showChar ':' .
>       ppPosn p .
>       ppNewLine .
>       showString xs

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
