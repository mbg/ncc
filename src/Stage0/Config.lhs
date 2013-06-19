{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

Configuration for the NCC build script.

> module Config (
>   Config(..),
>   getCfg
> ) where

    {----------------------------------------------------------------------}
    {-- Language extensions                                               -}
    {----------------------------------------------------------------------}

>   {-# LANGUAGE OverloadedStrings #-}

    {----------------------------------------------------------------------}
    {-- Module imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Control.Applicative
>   import Data.Monoid
>   import Data.Text.Lazy
>   import Options.Applicative
>   import Options.Applicative.Builder.Internal

    {----------------------------------------------------------------------}
    {-- Configuration data                                                -}
    {----------------------------------------------------------------------}  

>   data Config = Cfg {  
>       cfgSrc  :: Text,
>       cfgDist :: Text
>   } 

    {----------------------------------------------------------------------}
    {-- Configuration parsing                                             -}
    {----------------------------------------------------------------------}
    
    Sets a default value for an option of type Text.
    
>   tvalue :: String -> Mod f Text
>   tvalue = value . pack

>   treader = return . pack
    
>   parseCfg :: Parser Config
>   parseCfg = Cfg
>       <$> option (
>               long "src"     <> 
>               metavar "PATH" <> 
>               tvalue "src"   <>
>               reader treader <>
>               help "Path to the source directory"
>           )
>       <*> option (
>               long "dist"    <>
>               metavar "PATH" <>
>               tvalue "dist"  <>
>               reader treader <>
>               help "Path to the distribution directory"
>           )
    
>   getCfg :: IO Config
>   getCfg = execParser opts
>            where
>               opts = info parseCfg (fullDesc)    

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
