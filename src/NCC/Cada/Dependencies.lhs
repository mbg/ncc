{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

We do not support mutually recursive modules in Cada. This Haskell module contains functions which
extract import declerations from an AST and sort modules by dependencies into SCCs.

> module Cada.Dependencies (
>   SCC(..),
>   ModuleNode,
>   findDeps
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}

>   import Data.Graph
    
>   import Cada.AST
>   import Cada.Imports

>   import Compiler.Context

    {----------------------------------------------------------------------}
    {-- Dependencies                                                      -}
    {----------------------------------------------------------------------}
    
    A module node consists of the AST, the module name and a list of modules
    the module which is represented by the node depends on.
    
>   type ModuleNode = (Context, ModuleName, [ModuleName])

>   makeNode :: Context -> ModuleNode
>   makeNode ctx = (ctx, moduleName m, collectImports (moduleDefs m))
>       where
>           m = unL (ctxAST ctx)
    
>   findDeps :: [Context] -> [SCC ModuleNode]
>   findDeps = stronglyConnCompR . map makeNode

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
