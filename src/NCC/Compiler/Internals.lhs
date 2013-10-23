{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module Compiler.Internals (
>   internalModules,
>   isInternal,
>   preludeModules
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}

>   import qualified Data.Map as M
>   import Data.Maybe
    
>   import TypeSystem.Environments
    
>   import Internal.NCC
    
    {----------------------------------------------------------------------}
    {-- Internal Configuration                                            -}
    {----------------------------------------------------------------------}

>   internalModules :: M.Map String Envs
>   internalModules = M.fromList [
>       ("NCC.Internal", nccInternal)]

    Determines whether the module is wired into the compiler.
    
>   isInternal :: String -> Maybe Envs
>   isInternal m = M.lookup m internalModules

    {----------------------------------------------------------------------}
    {-- Standard library                                                  -}
    {----------------------------------------------------------------------}

>   preludeModules :: [String]
>   preludeModules = [
>       "NCC.Internal",
>       "Cada.Class.Default",
>       "Cada.Class.Eq",
>       "Cada.Class.Ord",
>       "Cada.Class.Num",
>       "Cada.Class.Show",
>       "Cada.Class.Functor",
>       "Cada.Class.Monad",
>       "Cada.Class.MonadTrans",
>       --"Cada.Class.Collection",
>       "Cada.Class.Monoid",
>       "Cada.Bool",
>       "Cada.Maybe",
>       "Cada.Either",
>       "Cada.List",
>       "Cada.Pair",
>       "Cada.IO",
>       "Cada.Monad.Identity",
>       "Cada.Monad.StateT",
>       "Cada"]
    
{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
