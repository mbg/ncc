{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

This module contains the data types which are used by Cada's lexer to represent input tokens. These
are then used by the parser to construct the abstract syntax tree.

> module Cada.Token (
>   TokenT(..),
>   Token(..),
>   isQualID,
>   isUnqualID
> ) where

    {----------------------------------------------------------------------}
    {-- Token Types                                                       -}
    {----------------------------------------------------------------------}
    
    This data type enumerates all types of lexical tokens in Cada.
    
>   data TokenT = TEoF
>               | TVar
>               | TCtr
>               | TVarSym
>               | TRes
>               | TRop
>               | TSpecial
>               | TInt
>               | TStr
>               deriving (Eq, Show)
    
    A token also has character data associated with it.
    
>   data Token = T {
>       tType :: TokenT,
>       tVal  :: String
>   } deriving Show

    Decides if an identifier is qualified. Qualified identifiers may
    contain dots so that we test if an identifier is qualified by 
    looking for a '.' character. Note that we do not use a separate 
    type of token for qualified identifiers, because we allow both
    qualified and unqualified identifiers in a lot of places. If we
    had two types of tokens, we would also need additional productions
    in the parser's grammar.

>   isQualID :: String -> Bool
>   isQualID = elem '.'

>   isUnqualID :: String -> Bool
>   isUnqualID = not . isQualID

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
