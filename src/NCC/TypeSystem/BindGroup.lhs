{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module TypeSystem.BindGroup (
>   Impl(..),
>   Expl(..),
>   Insts(..),
>   BindGroup(..),
>   enrichBG,
>   ppBindGroup
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import qualified Data.Map as M
    
>   import Utility.PrettyPrint
    
>   import Cada.AST (Equation)
>   import Cada.PrettyPrint (ppEquation)

>   import TypeSystem.Types
>   import TypeSystem.PolyType
>   import TypeSystem.Assump

    {----------------------------------------------------------------------}
    {-- Binding Groups                                                    -}
    {----------------------------------------------------------------------}

>   data Impl = Impl {
>       implName :: String,
>       implEqs  :: [Equation]
>   }

>   data Expl = Expl {
>       explName :: String,
>       explType :: PolyType,
>       explEqs  :: [Equation]
>   }

>   data Insts = Insts {
>       className :: String,
>       instHead  :: Qual MonoType,
>       instImpls :: [Expl]
>   }
    
>   data BindGroup = BG {
>       bgExpls :: [Expl],
>       bgImpls :: [[Impl]],
>       bgInsts :: [Insts]
>   }

>   implToEx :: Assumps -> Impl -> Expl
>   implToEx as (Impl n es) = case M.lookup n as of
>       (Just pt) -> Expl n pt es

>   enrichBG :: Assumps -> BindGroup -> BindGroup
>   enrichBG as (BG ex im is) = BG (ex++ex') [] []
>       where
>           ex' = concat $ map (map (implToEx as)) im
    
    {----------------------------------------------------------------------}
    {-- Pretty Printing                                                   -}
    {----------------------------------------------------------------------}
    
>   ppExpl :: Expl -> ShowS
>   ppExpl (Expl n t es) = 
>       showString n .
>       ppTyping .
>       ppPolyType t .
>       ppNewLine .
>       ppDefsW ppEquation ppNewLine es .
>       ppNewLine . ppNewLine

>   ppImpl :: Impl -> ShowS
>   ppImpl (Impl n es) = ppTab . ppDefsW ppEquation (ppNewLine . ppTab) es

>   ppImpls :: [Impl] -> ShowS
>   ppImpls is = 
>       ppInContext 0 (ppDefsW ppImpl ppNewLine is . ppNewLine) . 
>       ppNewLine

>   ppInsts :: Insts -> ShowS
>   ppInsts (Insts cls (ctx :=> mt) is) = 
>       ppContext ctx .
>       showString cls .
>       ppSpace .
>       ppMonoType 10 mt .
>       ppSpace . 
>       ppInContext 0 (ppDefsW ppExpl ppNewLine is . ppNewLine) .
>       ppNewLine

>   ppBindGroup :: BindGroup -> ShowS
>   ppBindGroup (BG es iss ins) =
>       showString "Explictly typed:\n" .
>       ppDefs ppExpl es .
>       ppNewLine .
>       showString "Implictly typed groups:\n" .
>       ppDefs ppImpls iss .
>       showString "Instance groups:\n" .
>       ppDefs ppInsts ins
    
{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
