{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

This module contains utility functions for pretty printing which are common to all other pretty
printing modules.

> module Utility.PrettyPrint (
>   ppDo,
>   ppSpace,
>   ppIndent,
>   ppTab,
>   ppNewLine,
>   ppInContext,
>   ppInParens,
>   ppInOptParens,
>   ppList,
>   ppSemicolon,
>   ppTyping,
>   ppDefs,
>   ppDefsW,
>   ppIf,
>   ppMaybe,
>   ppMany,
>   ppId,
>   ppIdStr
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}

    {----------------------------------------------------------------------}
    {-- Pretty Printing                                                   -}
    {----------------------------------------------------------------------}
    
>   ppDo :: ShowS -> String
>   ppDo f = f ""
    
>   ppSpace :: ShowS
>   ppSpace = showChar ' '

>   ppIndent :: Int -> ShowS
>   ppIndent 0 = id
>   ppIndent n = showString "    " . ppIndent (n-1)

>   ppTab :: ShowS
>   ppTab = ppIndent 1

>   ppNewLine :: ShowS
>   ppNewLine = showChar '\n'

>   ppInContext :: Int -> ShowS -> ShowS
>   ppInContext n f = 
>       showChar '{' . 
>       ppNewLine .
>       f .
>       ppIndent n .
>       showChar '}'

>   ppInParens :: ShowS -> ShowS
>   ppInParens f = showChar '(' . f . showChar ')'

>   ppInOptParens :: Bool -> ShowS -> ShowS
>   ppInOptParens True f = ppInParens f
>   ppInOptParens _    f = f

>   ppList :: ShowS -> ShowS
>   ppList f = showChar '[' . f . showChar ']'

>   ppSemicolon :: ShowS
>   ppSemicolon = showChar ';'

>   ppTyping :: ShowS
>   ppTyping = showString " :: "

>   ppDefs :: (a -> ShowS) -> [a] -> ShowS
>   ppDefs f = foldr (\x xs -> f x . xs) id 

>   ppDefsW :: (a -> ShowS) -> ShowS -> [a] -> ShowS
>   ppDefsW _ _ []     = id
>   ppDefsW f _ [x]    = f x
>   ppDefsW f g (x:xs) = f x . g . ppDefsW f g xs

>   ppIf :: Bool -> ShowS -> ShowS
>   ppIf False _ = id
>   ppIf True  f = f

>   ppMaybe :: (a -> ShowS) -> Maybe a -> ShowS
>   ppMaybe _ Nothing  = id
>   ppMaybe f (Just x) = f x 

>   ppMany :: ShowS -> Int -> ShowS
>   ppMany f 0 = id
>   ppMany f n = f . ppMany f (n-1)

>   ppId :: ShowS -> ShowS
>   ppId f = showChar '`' . f . showChar '\''

>   ppIdStr :: String -> ShowS
>   ppIdStr xs = ppId $ showString xs
    
{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
