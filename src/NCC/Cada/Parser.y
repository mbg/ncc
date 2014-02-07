{
{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

module Cada.Parser (
    parseModule,
    runCadaParser,
    runExprParser,
    hasErrors,
    unA
) where

{--------------------------------------------------------------------------}
{-- Module Imports                                                        -}
{--------------------------------------------------------------------------}

import Control.Applicative

import Utility.StateM

import Cada.Token
import Cada.Location
import Cada.ParserMonad
import Cada.ParserError
import Cada.ParserConstr
import Cada.Lexer
import Cada.AST
}

{--------------------------------------------------------------------------}
{-- Parser Configuration                                                  -}
{--------------------------------------------------------------------------}

%tokentype { TokenP }
%monad     { Parser } { >>= } { return }
%lexer     { lexer } { (_, T TEoF "") }
%error     { parseError }

%name parseModule module
%name parseExpr expr1

{--------------------------------------------------------------------------}
{-- Tokens                                                                -}
{--------------------------------------------------------------------------}

%token
    '{'          { ($$, T TSpecial "{")     }
    '}'          { ($$, T TSpecial "}")     }
    ';'          { ($$, T TSpecial ";")     }
    '('          { ($$, T TSpecial "(")     }
    ')'          { ($$, T TSpecial ")")     }
    '['          { ($$, T TSpecial "[")     }
    ']'          { ($$, T TSpecial "]")     }
    ','          { ($$, T TSpecial ",")     }
    '`'          { ($$, T TSpecial "`")     }
    '!'          { ($$, T TSpecial "!")     }
    '|'          { ($$, T TRop "|")      }
    '='          { ($$, T TRop "=")         }
    ':'          { ($$, T TRop ":")         }
    '->'         { ($$, T TRop "->")        }
    '<-'         { ($$, T TRop "<-")        }
    '::'         { ($$, T TRop "::")        }
    '=>'         { ($$, T TRop "=>")        }
    '\\'         { ($$, T TRop "\\")        }
    '<:'         { ($$, T TRop "<:")        }
    '>:'         { ($$, T TRop ">:")        }
    ':='         { ($$, T TRop ":=")        }
    VAR          { (_, T TVar _)            }
    CTR          { (_, T TCtr _)            }
    VSYM         { (_, T TVarSym _)         }
    INT          { (_, T TInt _)            }
    STR          { (_, T TStr _)            }
    CASE         { ($$, T TRes "case")      }
    CLASS        { ($$, T TRes "class")     }
    DATA         { ($$, T TRes "data")      }
    ELSE         { ($$, T TRes "else")      }
    ENUM         { ($$, T TRes "enum")      }
    IF           { ($$, T TRes "if")        }
    IN           { ($$, T TRes "in")        }
    INSTANCE     { ($$, T TRes "instance")  }
    IMPORT       { ($$, T TRes "import")    }
    LET          { ($$, T TRes "let")       }
    MODULE       { ($$, T TRes "module")    }
    NEWTYPE      { ($$, T TRes "newtype")   }
    OF           { ($$, T TRes "of")        }
    STATE        { ($$, T TRes "state")     }
    THEN         { ($$, T TRes "then")      }
    TYPE         { ($$, T TRes "type")      }
    '_'          { ($$, T TRes "_")         }
    ATUNIFY      { ($$, T TRes "@unify")    }
    TAGGED       { ($$, T TRes "tagged")    }

%left '->' THEN 
    
%%

{--------------------------------------------------------------------------}
{-- Misc                                                                  -}
{--------------------------------------------------------------------------}


{--------------------------------------------------------------------------}
{-- Operators                                                             -}
{--------------------------------------------------------------------------}
 
op :: { Loc String }
 : varop                                 { $1                             }
 
varop :: { Loc String }
 : '`' VAR '`'                           { tokenL $2                      }
 | VSYM                                  { tokenL $1                      }
 | ':'                                   { Loc ":" (FilePos $1)           }
 
{--------------------------------------------------------------------------}
{-- Literals                                                              -}
{--------------------------------------------------------------------------}

listlit :: { Literal }
 : '[' pexpr ']'                         { ListLit $2                     }

literal :: { Literal }
 : INT                                   { IntLit (tkVal $1)              }
 | STR                                   { StrLit (tkVal $1)              }
 | listlit                               { $1                             }

{--------------------------------------------------------------------------}
{-- Patterns                                                              -}
{--------------------------------------------------------------------------}

pattern :: { Pattern }
 : VAR                                   { VarPattern (tkVal $1)          }
 | CTR                                   { CtrPattern (tkVal $1) []       }
 | '(' ppat ')'                          { makeCtrPattern $2              }
 | literal                               { LitPattern $1                  }
 | '_'                                   { Wildcard                       }
 
ppat :: { [Pattern] }
 : ppat0                                 { unA $1                         }
 | ppat2 ':' ppat2                       { [CtrPattern ":" [$1,$3]]       }
 
ppat0 :: { Accum Pattern }   
 : {- epsilon -}                         { id                             }
 | ppat1                                 { $1                             }
 
ppat1 :: { Accum Pattern }
 : ppat2                                 { consA $1                       }
 | ppat1 ',' ppat2                       { contA $1 $3                    }
 
ppat2 :: { Pattern }
 : VAR                                   { VarPattern (tkVal $1)          }
 | CTR patterns                          { CtrPattern (tkVal $1) $2       }
 | '(' ppat ')'                          { makeCtrPattern $2              }
 | literal                               { LitPattern $1                  }
 | '_'                                   { Wildcard                       }
 
patterns :: { [Pattern] } 
 : patterns0                             { unA $1                         }
 
patterns0 :: { Accum Pattern }
 : patterns0 pattern                     { contA $1 $2                    }
 |                                       { id                             }

{--------------------------------------------------------------------------}
{-- Types                                                                 -}
{--------------------------------------------------------------------------}

ptypes :: { [SType] }
 : ptypes0                               { unA $1                         }
 
ptypes0 :: { Accum SType }   
 : type0                                 { consA $1                       }
 | ptypes0 ',' type0                     { contA $1 $3                    }
 
typeP :: { TypeParam }
 : VAR                                   {% makeTyParam $1                } 
 
typePs :: { [TypeParam] }
 : typePs0                               { unA $1                         }

typePs0 :: { Accum TypeParam }
 :                                       { id                             }
 | typePs0 typeP                         { contA $1 $2                    }

typeAnn :: { Maybe STypeAnn }
 :                                       { Nothing                        }
 | ATUNIFY type3 '=' STR                 { Just (STA $2 (tkVal $4))       }

type0 :: { SType }
 : type1 '->' type0                      {  tyArrow $1 $3                 }
 | type1                                 {  $1                            }

type1 :: { SType }
 : type2 typeAnn                         { mkTyAnn $1 $2                  }

type2 :: { SType }
 : type2 type3                           { STyApp $1 $2                   }
 | type3                                 { $1                             }
 
type3 :: { SType }
 : VAR                                   { STyVar (tkVal $1)              } 
 | '(' type0 ',' ptypes ')'              { STyTuple ($2 : $4)             }
 | '[' type0 ']'                         { STyList $2                     }
 | '[' ']'                               { STyCtr "[]"                    }
 | '(' ',' ')'                           { STyCtr "(,)"                   }
 | '(' type0 ')'                         { $2                             }
 | type4                                 { $1                             }

type4 :: { SType } 
 : '(' ')'                               {  tyUnit                        }
 | CTR                                   { STyCtr (tkVal $1)              }

{--------------------------------------------------------------------------}
{-- Contexts                                                              -}
{--------------------------------------------------------------------------}

context :: { [TypeConstraint] }
 : type2                                 {% makeContext $1                }
       
{--------------------------------------------------------------------------}
{-- Type Classes                                                          -}
{--------------------------------------------------------------------------}

name :: { Loc String }
 : VAR                                   { tokenL $1                      }
 | '(' VSYM ')'                          { tokenL $2                      }

names :: { Loc [String] }
 : names0                                { listL (unA $1)                 }

names0 :: { Accum (Loc String) }
 : name                                  { consA $1              }
 | names0 ',' name                       { contA $1 $3           }

funDecl :: { Loc DecType }
 : names '::' context '=>' type0 ';'     {% makeDefSig $1 $3 $5           }
 | names '::' type0 ';'                  {% makeDefSig $1 [] $3           }
   
funDecls :: { [Loc DecType] }
 : funDecls0                             { unA $1                         }
   
funDecls0 :: { Accum (Loc DecType) }
 :                                       { id                             }
 | funDecls0 funDecl                     { contA $1 $2                    }
      
tclass :: { STypeClass Loc }
 : context '=>' type2 '{' 
        funDecls 
   '}'                                   {% makeTypeClass $1 $3 $5        }
 | type2 '{' funDecls '}'                {% makeTypeClass [] $1 $3        }

instDecls :: { [InstDef Loc] }
 : instDecls0                            { unA $1                         }
 
instDecls0 :: { Accum (InstDef Loc) }
 :                                       { id                             }
 | instDecls0 instDecl                   { contA $1 $2                    }
 
instDecl :: { InstDef Loc }
 : funDecl                               { InstTyDef $1                   }
 | valDef                                { InstValDef (unL $1)            }
 
inst :: { SInstance Loc }
 : context '=>' type2 '{' 
        instDecls
   '}'                                   {% makeInstance $1 $3 $5         }
 | type2 '{' instDecls '}'               {% makeInstance [] $1 $3         }
      
{--------------------------------------------------------------------------}
{-- Data Types                                                            -}
{--------------------------------------------------------------------------}

ctrL :: { [Loc DataConstructor] }
     : CTR ',' ctrL                      {% makeDataCtrL $1 [] $3         }
     | CTR                               {% makeDataCtrL $1 [] []         }

dfield :: { DataField }
       : VAR '::' type0                  {% makeField $1 $3               }
  
fields :: { [DataField] }
 : fields0                               { unA $1                         }
  
fields0 :: { Accum DataField }
 :                                       { id                             }
 | fields0 dfield ';'                    { contA $1 $2                    }

ctrs :: { [Loc DataConstructor] }
     : CTR                               {% makeDataCtrL $1 [] []         }
     | CTR '{' fields '}' ctrs           {% makeDataCtrL $1 $3 $5         }
     | CTR ',' ctrs                      {% makeDataCtrL $1 [] $3         }
     | CTR '{' fields '}'                {% makeDataCtrL $1 $3 []         }
       
dataD :: { LocP Definition }
      : ENUM CTR '{' ctrL '}'            {% makeEnum $1 $2 $4             }
      | DATA CTR typePs '{' fields '}'   {% makeDataS $1 $2 $3 $5         }
      | DATA CTR typePs '{' ctrs '}'     {% makeData $1 $2 $3 $5          }
      | NEWTYPE CTR typePs '=' CTR
            '{' dfield '}'               {% makeDataN $1 $2 $3 $5 $7      }

{--------------------------------------------------------------------------}
{-- State                                                                 -}
{--------------------------------------------------------------------------}

opt_default :: { Expr }
 : '=' expr0                             { $2                             }
 |                                       { Var "undefined"                }

sfield :: { StateDataField }
       : VAR opt_default '::' type0      {% makeSField $1 $2 $4           }
  
sfields :: { [StateDataField] }
 : sfields0                              { unA $1                         }
  
sfields0 :: { Accum StateDataField }
 :                                       { id                             }
 | sfields0 sfield ';'                   { contA $1 $2                    }

stateP :: { Maybe SType }
 :                                       { Nothing                        }
 | ':' type2                             { Just $2                        }
 
stateD :: { LocP Definition }
 : STATE CTR typePs stateP 
   '{' 
     sfields
   '}'                                   {% makeState $1 $2 $3 $4 $6      }
 
{--------------------------------------------------------------------------}
{-- Tagged                                                                -}
{--------------------------------------------------------------------------}

relation :: { Relation }
 : VAR '->' VAR                          { Rel [tokenL $1] [tokenL $3] }

relations0 :: { Accum Relation }
 : relation                              { consA $1                      }
 | relations0 ',' relation               { contA $1 $3                   }

relations :: { [Relation] }
 : relations0                            { unA $1                        }

tagrule :: { TaggedRule }
 : CTR patterns '::' type0               { TagRule $2 $4 }

tagrules0 :: { Accum TaggedRule }
 :                                       { id                            }
 | tagrules0 tagrule ';'                 { contA $1 $2                   }

tagrules :: { [TaggedRule] }
 : tagrules0                             { unA $1                        }

tagdef :: { LocP Definition }
 : TAGGED CTR typePs '|' relations '{' tagrules '}' {% makeTagged $1 $2 $3 $5 $7 }

{--------------------------------------------------------------------------}
{-- Expressions                                                           -}
{--------------------------------------------------------------------------}
      
alt :: { Alt }
 : patterns '=' expr0                    { Alt $1 $3                      }
 
alts :: { [Alt] }    
 : alts0                                 { unA $1                         }
 
alts0 :: { Accum Alt }
 : alt                                   { consA $1                       }
 | alts0 ';' alt                         { contA $1 $3                    }
      
option :: { Option }
 : pattern '->' expr0 ';'                { Option $1 $3                   }
      
options :: { [Option] }
 : options0                              { unA $1                         }
      
options0 :: { Accum Option }
 : option                                { consA $1                       }
 | options0 option                       { contA $1 $2                    }

stmts :: { [Statement] }
 : stmts0                                { unA $1                         }
 
stmts0 :: { Accum Statement }
 :                                       { id                             }
 | stmts0 stmt                           { contA $1 $2                    }
 
stmt :: { Statement }
 : expr0 ';'                             { Statement $1                   }
 | expr2 '<-' expr0 ';'                  {% makeBind $1 $3                }
 | expr2 '<:' VAR ';'                    {% makeGetter $1 $3              }
 | expr0 '>:' VAR ';'                    {% makeSetter' $1 $3             }
 | expr2 ':=' expr0 ';'                  {% makeSetter $3 $1              }

expr0 :: { Expr }
 : expr1                                 { $1                             }
 | expr0 op expr1                        { InfixOp (unL $2) $1 $3         }
 
expr1 :: { Expr }
 : '\\' patterns '->' expr0              { Abs (Alt $2 $4)                }
 | CASE expr0 OF '{' options '}'         { Case $2 $5                     }
 | IF expr0 THEN expr0 ELSE expr0        {% makeCond $2 $4 $6             }
 | LET alts IN expr0                     {% makeLet $1 $2 $4              }
 | expr2                                 { $1                             }

expr2 :: { Expr }
 : expr2 atom                            { App $1 $2                      }
 | atom                                  { $1                             }
      
atom :: { Expr }      
 : VAR                                   { Var (tkVal $1)                 }
 | CTR                                   { Ctr (tkVal $1)                 }
 | '{' stmts '}'                         {% checkStmts $2                 }
 | literal                               { Lit $1                         }
 | '(' pexpr ')'                         { makeParensExpr $2              }
      
pexpr :: { [Expr] }
 : pexpr0                                { unA $1                         }
 
pexpr0 :: { Accum Expr }   
 : {- epsilon -}                         { id                             }
 | pexpr1                                { $1                             }
 | varop                                 { consA (Var (unL $1))           }
 
pexpr1 :: { Accum Expr }
 : expr0                                 { consA $1                       }
 | pexpr1 ',' expr0                      { contA $1 $3                    }
      
{--------------------------------------------------------------------------}
{-- Definitions                                                           -}
{--------------------------------------------------------------------------}

valDef :: { Loc Equation }
 : name patterns '=' expr0 ';'           {% makeEquation $1 $2 $4         }
 | pattern VSYM patterns '=' expr0 ';'   {% makeEquation (tokenL $2) ($1:$3) $5 }
      
{--------------------------------------------------------------------------}
{-- Modules                                                               -}
{--------------------------------------------------------------------------}
    
module :: { LocP Module }
 : MODULE CTR '{' defs '}'               {% makeModule $1 $2 $4           }
       
defs :: { [LocP Definition] }
 : defs0                                 { unA $1                         }

defs0 :: { Accum (LocP Definition) }
 :                                       { id                             }
 | defs0 def                             { contA $1 $2                    }
      
def :: { LocP Definition }
    : IMPORT CTR ';'                     {% makeImportDef $1 $2           }
    | TYPE CTR typePs '=' type0 ';'      {% makeTypeDef $1 $2 $3 $5       }
    | CLASS tclass                       {% makeTyClDef $1 $2             }
    | INSTANCE inst                      {% makeInstDef $1 $2             }
    | dataD                              {  $1                            }
    | stateD                             {  $1                            }
    | funDecl                            { makeTypeDec $1                 }
    | valDef                             {% makeValDef $1                 }
    | tagdef                             {  $1 }
{
{--------------------------------------------------------------------------}
{-- Auxiliary Parser Functions                                            -}
{--------------------------------------------------------------------------}
    
debug :: String -> a -> Parser a
debug m x = do
    p <- getPosP
    addInfoP (FilePos p) m
    return x
    
lexer :: (TokenP -> Parser a) -> Parser a
lexer f = P (\s -> case runAlexFrom alexMonadScan s of
    (Left err)      -> lexicalError err s >> failMT
    (Right (s', t)) -> let (P m) = f t in {-addInfo s' (show t) >>-} m s')

runCadaParser :: String -> (Maybe (LocP Module), ErrorS)
runCadaParser xs = runStateM (initParser parseModule xs) id

runExprParser :: String -> (Maybe Expr, ErrorS)
runExprParser xs = runStateM (initParser parseExpr xs) id

parseError :: TokenP -> Parser a
parseError (p,t) = failP p (ppParserError t)

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}
}
