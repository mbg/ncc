{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module TypeSystem.Assump (
>   Assumps,
>   unions,
>   fromList,
>   mkAssumps,
>   bindingNames,
>   bindingTypes,
>   (~=),
>   empty,
>   find,
>   (<>),
>   ppAssumps
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Control.Monad.Error
>   import qualified Data.Map as M
    
>   import TypeSystem.PolyType
>   import TypeSystem.TypeError

>   import Utility.PrettyPrint

    {----------------------------------------------------------------------}
    {-- Assumptions                                                       -}
    {----------------------------------------------------------------------}
   
>   type Assumps = M.Map String PolyType

>   unions :: [Assumps] -> Assumps
>   unions = foldl M.union M.empty 

>   fromList :: [(String, PolyType)] -> Assumps
>   fromList = M.fromList 

>   mkAssumps :: [String] -> [PolyType] -> Assumps
>   mkAssumps ns ps = M.fromList $ zip ns ps

>   bindingNames :: Assumps -> [String]
>   bindingNames = M.keys

>   bindingTypes :: Assumps -> [PolyType]
>   bindingTypes = M.elems

>   (~=) :: String -> PolyType -> Assumps
>   n ~= t = M.singleton n t

>   empty :: Assumps
>   empty = M.empty

>   find :: Monad m => String -> Assumps -> ErrorT TypeError m PolyType
>   find n as = case M.lookup n as of
>       (Just t) -> return t
>       Nothing  -> throwError $ NotInScope n

>   (<>) :: Assumps -> Assumps -> Assumps
>   (<>) = M.union
    
    {----------------------------------------------------------------------}
    {-- Pretty Printing                                                   -}
    {----------------------------------------------------------------------}
    
>   ppAssump :: Int -> String -> PolyType -> ShowS -> ShowS
>   ppAssump i n t c = 
>       ppIndent i .
>       showString n .
>       ppTyping .
>       ppPolyType t .
>       ppNewLine .
>       c
    
>   ppAssumps :: Int -> Assumps -> ShowS
>   ppAssumps i = M.foldrWithKey (ppAssump i) id
    
{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
