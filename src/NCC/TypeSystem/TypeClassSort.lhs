{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

The type class hierarchy may not be cyclic. This module contains functions which sort type classes
into strongly-connected components.

> module TypeSystem.TypeClassSort (
>   SCC(..),
>   ClassNode,
>   clDef,
>   clName,
>   sortClasses
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Data.Graph 
>   import qualified Data.Map as M
>   import qualified Data.Set as S

>   import Cada.Location

>   import TypeSystem.Types
>   import TypeSystem.TypeClass
>   import TypeSystem.Environments

>   import Utility.Accum

    {----------------------------------------------------------------------}
    {-- Sorting                                                           -}
    {----------------------------------------------------------------------}

>   type ClassNode = (TypeClass, String, [String])

>   clDef :: ClassNode -> TypeClass
>   clDef (cls, _, _) = cls

>   clName :: ClassNode -> String
>   clName (_, n, _) = n

>   supers :: Context -> [String]
>   supers = S.foldr (\(In n _) ns -> n : ns) []

>   toClNode :: String -> TypeClass -> [ClassNode] -> [ClassNode]
>   toClNode n cl@(TypeClass ctx _ _) ns = (cl, n, supers ctx) : ns

>   sortClasses :: ClEnv -> [SCC ClassNode]
>   sortClasses = stronglyConnCompR . M.foldrWithKey toClNode []
    
{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
