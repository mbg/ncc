{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

This module contains Cada's parser monad which is actually a monad transformer on top of 

> module Cada.ParserMonad (
>   Accum(..),
>   unA,
>   consA,
>   contA,
>   ParserMsg(..),
>   ErrorS,
>   Parser(..),
>   failP,
>   initParser,
>   hasErrors,
>   addError,
>   addErrorP,
>   addWarning,
>   addInfo,
>   addInfoP,
>   getPosP,
>   lexicalError
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Utility.Accum
>   import Utility.StateM
>   import Utility.PrettyPrint

>   import Cada.Lexer
>   import Cada.Pos
>   import Cada.ParserMsg

    {----------------------------------------------------------------------}
    {-- Parser Monad                                                      -}
    {----------------------------------------------------------------------}

>   type ErrorS      = Accum ParserMsg
>   newtype Parser a = P { runParser :: AlexState -> StateM ErrorS a }

>   instance Functor Parser where
>       fmap f (P m) = P (\s -> do 
>           x <- m s
>           return (f x))

>   instance Monad Parser where
>       return x    = P (\_ -> return x)
>       (P m) >>= f = P (\s -> do 
>           x <- m s
>           let (P m') = f x in m' s)
>       fail msg    = P (\s -> addError s msg >> failMT) 

>   failP :: AlexPosn -> String -> Parser a
>   failP p m = P (\_ -> do
>       modify $ flip contA (PM Error (FilePos p) m)
>       failMT)

>   initialState :: String -> AlexState
>   initialState xs = AlexState {
>       alex_pos = AlexPn 0 1 1,
>       alex_inp = xs,       
>       alex_chr = '\n',
>       alex_bytes = [],
>       alex_scd = 0
>   }

>   initParser :: Parser a -> String -> StateM ErrorS a
>   initParser (P m) xs = m (initialState xs)

>   hasErrors :: [ParserMsg] -> Bool
>   hasErrors = any isParserError

>   makeMsg :: MsgLevel -> AlexState -> String -> ParserMsg
>   makeMsg l s m = PM l (FilePos (alex_pos s)) m

>   addAux :: MsgLevel -> AlexState -> String -> StateM ErrorS ()
>   addAux l s m = modify $ flip contA (makeMsg l s m)

>   addError :: AlexState -> String -> StateM ErrorS ()
>   addError = addAux Error

>   addErrorP :: Pos -> String -> Parser ()
>   addErrorP p m = P (\s -> modify $ flip contA (PM Error p m))

>   addWarning :: AlexState -> String -> StateM ErrorS ()
>   addWarning = addAux Warn 

>   addInfo :: AlexState -> String -> StateM ErrorS ()
>   addInfo = addAux Info

>   addInfoP :: Pos -> String -> Parser ()
>   addInfoP p m = P (\s -> do
>       c <- get
>       put (\ms -> c (PM Info p m : ms)))

>   getPosP :: Parser AlexPosn
>   getPosP = P $ \s -> do
>       return $ alex_pos s

    {----------------------------------------------------------------------}
    {-- Lexical Errors                                                    -}
    {----------------------------------------------------------------------}

>   ppLexicalError :: String -> AlexState -> String
>   ppLexicalError msg s = ppDo $
>       showString msg .
>       showString " near input " . 
>       ppIdStr (take 10 (alex_inp s))

>   lexicalError :: String -> AlexState -> StateM ErrorS ()
>   lexicalError msg s = addError s (ppLexicalError msg s)
    
{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
