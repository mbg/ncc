{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module Compiler.Context (
>   Context(..),
>   sourceDir,
>   ifaceName
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}

>   import System.FilePath
    
>   import Cada.AST

>   import Utility.IO
    
    {----------------------------------------------------------------------}
    {-- Contexts                                                          -}
    {----------------------------------------------------------------------}
    
>   data Context = Ctx {
>       ctxFn  :: FilePath,
>       ctxAST :: LocP Module
>   }

>   sourceDir :: FilePath -> FilePath
>   sourceDir = takeDirectory 

>   ifaceName :: Context -> FilePath
>   ifaceName = swapExtension ".co" . ctxFn 

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
