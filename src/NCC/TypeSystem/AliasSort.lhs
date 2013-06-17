{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

Type functions must be resolved in order of dependency and may not contain cycles. This module
takes care of sorting them into strongly connected components.

> module TypeSystem.AliasSort (
>   SCC(..),
>   AliasNode,
>   anDef,
>   anLoc,
>   sortAliases
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Data.Graph 

>   import Cada.Location
>   import Cada.STypes
>   import Cada.AST

>   import Utility.Accum

    {----------------------------------------------------------------------}
    {-- Sorting                                                           -}
    {----------------------------------------------------------------------}

>   type AliasNode = (Loc AliasDefinition, String, [String])

>   anDef :: AliasNode -> Loc AliasDefinition
>   anDef (d,_,_) = d

>   anLoc :: AliasNode -> Pos
>   anLoc (Loc _ p,_,_) = p

>   extractAl :: LocP Definition -> Accum (Loc AliasDefinition)
>   extractAl (Loc (TypeDef d)  p) = consA (Loc d p)
>   extractAl (Loc (StateDef d) p) = consA (Loc (sDefType d) p)
>   extractAl _                    = id

>   filterAl :: [LocP Definition] -> [Loc AliasDefinition]
>   filterAl = foldr extractAl []

>   toAlNode :: Loc AliasDefinition -> AliasNode
>   toAlNode al@(Loc d _) = (al, aDefName d, ctrsFromType (aDefType d) [])

>   sortAliases :: [LocP Definition] -> [SCC AliasNode]
>   sortAliases = stronglyConnCompR . map toAlNode . filterAl
    
{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
