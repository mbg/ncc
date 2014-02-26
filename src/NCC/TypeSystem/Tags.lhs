{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module TypeSystem.Tags (
>   TagRule(..),
>   Tags,
>   patternEq,
>   tagRuleEq,
>   checkTagOverlap,
>   checkTagOverlaps,
>   findRule,
>   ppTagRule
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}

>   import Debug.Trace (trace)

>   import qualified Data.Map as M

>   import Utility.PrettyPrint

>   import Cada.AST (Pattern(..), Literal(..))
>   import Cada.PrettyPrint

>   import TypeSystem.Kind
>   import TypeSystem.Types
>   import TypeSystem.PolyType

    {----------------------------------------------------------------------}
    {-- Tag rules                                                         -}
    {----------------------------------------------------------------------}

>   data TagRule = TagRule { tagRulePattern :: [Pattern], tagRuleType :: PolyType }

    {----------------------------------------------------------------------}
    {-- Tags                                                              -}
    {----------------------------------------------------------------------}

>   type Tags = [TagRule]

>   literalEq :: Literal -> Literal -> Bool
>   literalEq UnitLit    UnitLit    = trace "unit match" $ True
>   literalEq (IntLit x) (IntLit y) = trace ("int comp:" ++ show x ++ "," ++ show y) $ x == y
>   literalEq (StrLit x) (StrLit y) = trace ("str comp:" ++ show x ++ "," ++ show y) $ x == y
>   literalEq _          _          = trace "no match" $ False

>   patternEq :: Pattern -> Pattern -> Bool
>   patternEq (LitPattern x) (LitPattern y) = literalEq x y
>   patternEq x              y              = trace ("pattern mismatch: " ++ ppDo (ppPattern x)) $ False

>   tagRuleEq :: [Pattern] -> TagRule -> Bool
>   tagRuleEq ps (TagRule ps' _) 
>       | length ps /= length ps' = trace "length doesn't match" $ False
>       | otherwise               = or $ map (uncurry patternEq) (zip ps ps')

>   checkTagOverlap :: Tags -> TagRule -> Bool
>   checkTagOverlap xs (TagRule ps _) = or $ map (tagRuleEq ps) xs

>   checkTagOverlaps :: Tags -> Tags -> Bool
>   checkTagOverlaps xs ys = or $ map (checkTagOverlap xs) ys

>   findRule :: Tags -> [Pattern] -> Maybe TagRule
>   findRule [] ps = Nothing
>   findRule (x@(TagRule ps' _):xs) ps 
>       | tagRuleEq (take (length ps') ps) x = Just x
>       | otherwise                          = findRule xs ps

>   ppTagRule :: TagRule -> ShowS
>   ppTagRule (TagRule ps mt) = ppPatterns ps . ppTyping
