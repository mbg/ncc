{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module Compiler.Configuration (
>   Config(..),
>   getCfg
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Data.Monoid
>   import Options.Applicative
    
    {----------------------------------------------------------------------}
    {-- Configuration Type                                                -}
    {----------------------------------------------------------------------}  

>   data Config = Cfg {  
>       cfgInputs           :: [String],
>       cfgOutput           :: String,
>       cfgEntry            :: String,
>       cfgPaths            :: [String],
>       cfgUseStdout        :: Bool,
>       cfgTrace            :: Bool,
>       cfgNoLinking        :: Bool,
>       cfgStopAfterParser  :: Bool,
>       cfgKeepHs           :: Bool,
>       cfgNoImplictPrelude :: Bool
>   }  
    
    {----------------------------------------------------------------------}
    {-- Configuration Parsing                                             -}
    {----------------------------------------------------------------------} 

>   strOptions = many . strOption

    Parses the compiler configuration.
    
>   parseCfg :: Parser Config
>   parseCfg = Cfg
>       <$> arguments str (metavar "FILE(s)")
>       <*> strOption (
>               long "output"  <> 
>               short 'o'      <>
>               metavar "FILE" <> 
>               value "a.exe"  <>
>               help "Name of the output file"
>           )
>       <*> strOption (
>               long "entry"     <>
>               short 'e'        <>
>               metavar "MODULE" <>
>               value "Main"     <>
>               help "Name of the module which contains the entry point"
>           )
>       <*> strOptions (
>               long "path"    <>
>               short 'p'      <>
>               metavar "PATH" <> 
>               help "Path(s) to search in" 
>           )
>       <*> switch (
>               long "use-stdout" <>
>               value False       <>
>               help "Redirect output to stdout when possible"
>           )
>       <*> switch (
>               long "trace" <> 
>               value False  <>
>               help "Enable debugging information"
>           )
>       <*> switch (
>               long "no-link" <>
>               value False    <>
>               help "Do not link object files into an executable"
>           )
>       <*> switch (
>               long "stop-after-parser" <>
>               value False              <>
>               help "Stops after each input file has been parsed"
>           )
>       <*> switch (
>               long "keep-hs" <>
>               value False    <>
>               help "Keeps intermediate .hs files"
>           )
>       <*> switch (
>               long "no-implicit-prelude" <>
>               value False               <>
>               help "If enabled, core modules won't be imported automatically"
>           )

>   getCfg :: IO Config
>   getCfg = execParser opts
>            where
>               opts = info parseCfg (fullDesc)
    
{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
