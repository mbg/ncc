{
{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

module Cada.Lexer (
    AlexPosn(..),
    AlexState(..),
    TokenT(..),
    Token(..),
    TokenP,
    tkVal,
    runAlexFrom, 
    alexMonadScan,
    skip
) where

import Control.Monad (liftM)
import Data.Char (chr)

import Cada.Token
}

%wrapper "monad"

$digit     = 0-9	
$lower     = a-z
$upper     = A-Z
$symbol    = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$special   = [\(\)\,\;\[\]\`\{\}\!]
$graphic   = [$lower $upper $symbol $digit $special \:\"\']
$whitechar = [\r\n\t\v\ ]
$slany     = [$graphic \t\ ]
$mlany     = [$graphic $whitechar]
$newline   = [\r\n] 
$idchar    = [$lower $upper $digit \'\_]
$rwhite    = $whitechar # \n
$chesc     = [abfnrtv\\\"\'\&]

@rid       = "case" 
           | "class" 
           | "data" 
           | "else"
           | "enum"
           | "if"
           | "import" 
           | "in" 
           | "instance" 
           | "let" 
           | "module" 
           | "newtype"
           | "state" 
           | "then" 
           | "type" 
           | "_"
           | "@unify"
           | "tagged"

@rop       = ".."
           | ":"
           | "::"
           | "="
           | "\"
           | "|"
           | "<-"
           | "->"
           | "=>"
           | "<:"
           | ">:"
           | ":="
           
@var       = $lower $idchar*
@ctr       = $upper $idchar*
@vsym      = $symbol [$symbol \:]*
@dec       = $digit+
@esc       = \\ $chesc
@str       = $graphic # [\"\\] | " " | @esc
           
cada :-

  $white+	    	            { skip               }
  "--"\-*[^$symbol].*	        { skip               }
  "{-"                          { mlComment          }
  @rid                          { makeToken TRes     }
  @rop                          { makeToken TRop     }
  (@ctr \.)* (@var \.)* @var    { makeToken TVar     }
  (@ctr \.)* @ctr               { makeToken TCtr     }
  $special                      { makeToken TSpecial }
  @vsym                         { makeToken TVarSym  }
  @dec                          { makeToken TInt     }
  \" @str* \"                   { makeToken TStr     }
{

type LexerAction a = AlexInput -> Int -> Alex a
type TokenP        = (AlexPosn, Token)

tkVal :: TokenP -> String
tkVal = tVal . snd

{--------------------------------------------------------------------------}
{-- Token Construction                                                    -}
{--------------------------------------------------------------------------}

makeToken :: TokenT -> LexerAction TokenP
makeToken t (p, _, _, xs) n = return $ (p, T t (take n xs)) 

{--------------------------------------------------------------------------}
{-- Nested Multiline Comments                                             -}
{--------------------------------------------------------------------------}

mlError :: AlexInput -> Alex TokenP
mlError inp = do
    alexSetInput inp
    alexError "Unterminated multi-line comment"

recComments :: Int -> AlexInput -> Alex TokenP
recComments 0 inp = alexSetInput inp >> alexMonadScan
recComments n inp = case alexGetByte inp of
    Nothing      -> mlError inp
    Just (c,inp) -> case chr (fromIntegral c) of
        '-' -> case alexGetByte inp of
            Nothing        -> mlError inp
            Just (125,inp) -> recComments (n-1) inp
            Just (c,inp)   -> recComments n inp
        '{' -> case alexGetByte inp of
            Nothing      -> mlError inp
            Just (c,inp) | c == fromIntegral (ord '-') -> recComments (n+1) inp
            Just (c,inp) -> recComments n inp
        c   -> recComments n inp
   
mlComment :: LexerAction TokenP
mlComment _ _ = alexGetInput >>= recComments 1

{--------------------------------------------------------------------------}
{-- End of File functions                                                 -}
{--------------------------------------------------------------------------}
    
alexEOF :: Alex TokenP
alexEOF = return (AlexPn 0 0 0, T TEoF "")

isEOF :: TokenT -> Bool
isEOF = (==) TEoF

{--------------------------------------------------------------------------}
{-- External interface                                                    -}
{--------------------------------------------------------------------------}

runAlexFrom :: Alex a -> AlexState -> Either String (AlexState, a)
runAlexFrom (Alex m) s = m s

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}
}
