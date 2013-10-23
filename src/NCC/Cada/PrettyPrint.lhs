{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module Cada.PrettyPrint (
>   ppDo,
>   ppParserMsg,
>   ppType,
>   ppOption,
>   ppExpr,
>   ppStatement,
>   ppPattern,
>   ppAlt,
>   ppEquation',
>   ppEquation,
>   ppAST
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Utility.PrettyPrint

>   import Cada.Location
>   import Cada.ParserMsg
>   import Cada.AST 

>   import TypeSystem.Kind

    {----------------------------------------------------------------------}
    {-- Parser Messages                                                   -}
    {----------------------------------------------------------------------}

>   ppMsgLevel :: MsgLevel -> ShowS
>   ppMsgLevel Info  = showString "Info"
>   ppMsgLevel Warn  = showString "Warning"
>   ppMsgLevel Error = showString "Error"
    
>   ppParserMsg :: ParserMsg -> ShowS
>   ppParserMsg m = 
>       ppMsgLevel (msgLevel m) .
>       showString " in " .
>       ppPosn (msgPos m) .
>       showString ":\n\t" .
>       showString (msgDesc m) .
>       ppNewLine

    {----------------------------------------------------------------------}
    {-- Abstract Syntax Tree                                              -}
    {----------------------------------------------------------------------}

>   ppNames :: [String] -> ShowS
>   ppNames []     = id
>   ppNames [x]    = showString x
>   ppNames (x:xs) = showString x . showChar ',' . ppNames xs 
    
>   ppAST :: LocP Module -> String
>   ppAST m = ppModule m ""

    {----------------------------------------------------------------------}
    {-- Patterns                                                          -}
    {----------------------------------------------------------------------}

>   ppPattern :: Pattern -> ShowS
>   ppPattern Wildcard          = showChar '_'
>   ppPattern (VarPattern x)    = showString x
>   ppPattern (LitPattern l)    = ppLiteral l
>   ppPattern (CtrPattern c []) = showString c
>   ppPattern (CtrPattern ":" [x,y]) = ppInParens $ 
>       ppPattern x . showString " : " . ppPattern y
>   ppPattern (CtrPattern c ps) = ppInParens $ 
>       showString c .
>       ppSpace .
>       ppPatterns ps

>   ppPatterns :: [Pattern] -> ShowS
>   ppPatterns = ppDefsW ppPattern ppSpace

    {----------------------------------------------------------------------}
    {-- Types & Kinds                                                     -}
    {----------------------------------------------------------------------}

>   ppKindSig :: Kind -> ShowS
>   ppKindSig k = ppTyping . ppKind False k
    
>   ppPadType :: SType -> ShowS
>   ppPadType t = ppSpace . ppType' 5 t

>   ppType' :: Int -> SType -> ShowS
>   ppType' w (STyVar x)    = showString x
>   ppType' w (STyCtr x)    = showString x
>   ppType' w (STyTuple ts) = ppInParens $ ppDefsW ppType (showChar ',') ts
>   ppType' w (STyList t)   = ppList (ppType t)
>   ppType' w (STyApp (STyApp (STyCtr "->") l) r)  = ppInOptParens (w > 0) $
>       ppType' 5 l .
>       showString " -> " .
>       ppType' 0 r
>   ppType' w (STyApp l r)  = ppInOptParens (w > 5) $ 
>       ppType' 5 l . 
>       ppSpace . 
>       ppType' 10 r

>   ppType :: SType -> ShowS
>   ppType = ppType' 0

>   ppQType :: TyQual SType -> ShowS
>   ppQType (ctx :==> t) = ppType t

>   ppTypeS :: TyScheme -> ShowS
>   ppTypeS (Scheme [] qt) = ppQType qt
>   ppTypeS (Scheme xs qt) = 
>       showString "forall " .
>       ppDefsW showString ppSpace xs .
>       showChar '.' .
>       ppQType qt

    {----------------------------------------------------------------------}
    {-- Type Classes                                                      -}
    {----------------------------------------------------------------------}

>   ppTypeClass :: STypeClass Loc -> ShowS
>   ppTypeClass (TyClass cs n ps ds) =
>       showString "class " .
>       ppTypeConstrs cs .
>       showString n .
>       ppSpace .
>       ppDefsW showString ppSpace ps .
>       ppSpace .
>       ppInContext 1 (ppDefs ppLDecType ds)

>   ppLDecType :: Loc DecType -> ShowS
>   ppLDecType (Loc v p) =
>       ppIndent 2 .
>       ppPosn p .
>       ppSpace .
>       ppDecType v .
>       ppNewLine

>   ppInstDef :: InstDef Loc -> ShowS
>   ppInstDef (InstTyDef d) = 
>       ppIndent 2 .
>       ppDecType (unL d) .
>       ppNewLine
>   ppInstDef (InstValDef d) =
>       ppIndent 2 .
>       ppEquation d .
>       ppNewLine

>   ppInstance :: SInstance Loc -> ShowS
>   ppInstance (SInst cs n t ds) =
>       showString "instance " .
>       ppTypeConstrs cs .
>       showString n .
>       ppSpace .
>       ppType' 10 t .
>       ppSpace .
>       ppInContext 1 (ppDefs ppInstDef ds)

>   ppTypeConstrL :: [TypeConstraint] -> ShowS
>   ppTypeConstrL [x]    = ppTypeConstr x
>   ppTypeConstrL (x:xs) = ppTypeConstr x . showString ", " . ppTypeConstrL xs

>   ppTypeConstrs :: [TypeConstraint] -> ShowS
>   ppTypeConstrs []  = id
>   ppTypeConstrs [x] = ppTypeConstr x . showString " => "
>   ppTypeConstrs xs  = ppInParens (ppTypeConstrL xs) . showString " => "

>   ppTypeConstr :: TypeConstraint -> ShowS
>   ppTypeConstr (TyConstr t v) =
>       showString t .
>       ppSpace .
>       showString v

>   ppTypeParam :: TypeParam -> ShowS
>   ppTypeParam (TyP n p) = showString n . ppSpace

>   ppDataFields :: [DataField] -> ShowS
>   ppDataFields []     = id
>   ppDataFields [c]    = ppDataField c . ppNewLine
>   ppDataFields (c:cs) = ppDataField c . showChar ',' . ppNewLine . ppDataFields cs

>   ppDataField :: DataField -> ShowS
>   ppDataField (DField n t p) = 
>       ppIndent 3 . 
>       ppPosn (FilePos p) .
>       ppSpace .
>       showString n . 
>       ppTyping . 
>       ppType t

>   ppStateDataFields :: [StateDataField] -> ShowS
>   ppStateDataFields []     = id
>   ppStateDataFields [c]    = ppStateDataField c . ppNewLine
>   ppStateDataFields (c:cs) = ppStateDataField c . showChar ',' . ppNewLine . ppStateDataFields cs

>   ppStateDataField :: StateDataField -> ShowS
>   ppStateDataField (SField n e t p) = 
>       ppIndent 3 . 
>       ppPosn (FilePos p) .
>       ppSpace .
>       showString n . 
>       showString " = " .
>       ppExpr 4 e .
>       ppTyping . 
>       ppType t

>   ppDataCtrs :: [Loc (Typed DataConstructor)] -> ShowS
>   ppDataCtrs []     = id
>   ppDataCtrs [c]    = ppDataCtr (unL c) . ppNewLine
>   ppDataCtrs (c:cs) = ppDataCtr (unL c) . showChar ',' . ppNewLine . ppDataCtrs cs

>   ppDataCtr :: Typed DataConstructor -> ShowS
>   ppDataCtr (Ty (DCtr n fs) ts) =
>       ppIndent 2 .
>       showString n .
>       ppTyping .
>       ppTypeS ts .
>       ppSpace . 
>       ppInContext 2 (ppDataFields fs)

>   ppDataCtrName :: Typed DataConstructor -> ShowS
>   ppDataCtrName (Ty (DCtr n _) ts) =
>       ppIndent 2 .
>       showString n .
>       ppTyping .
>       ppTypeS ts 

>   ppDataDef :: DataDefinition -> ShowS
>   ppDataDef (DDef Data n ps cs fs) = 
>       showString "data " .
>       showString n .
>       ppSpace .
>       ppDefs ppTypeParam ps .
>       ppInContext 1 (ppDataCtrs cs) .
>       ppNewLine .
>       ppDefsW (ppEquation' 1) ppNewLine fs
>   ppDataDef (DDef Single n ps [c] fs) =
>       showString "data " .
>       showString n .
>       ppSpace .
>       ppDefs ppTypeParam ps .
>       ppInContext 1 (ppDataFields (dCtrFields $ typedVal (unL c))) .
>       ppNewLine .
>       ppDefsW (ppEquation' 1) ppNewLine fs
>   ppDataDef (DDef Newtype n ps [c] fs) = let
>           uc = typedVal $ unL c in
>       showString "newtype " .
>       showString n .
>       ppSpace .
>       ppDefs ppTypeParam ps .
>       showString "= " .
>       showString (dCtrName uc) .
>       ppTyping .
>       ppTypeS (typedType $ unL c) .
>       ppSpace .
>       ppInContext 1 (ppDataFields (dCtrFields uc)) .
>       ppNewLine .
>       ppDefsW (ppEquation' 1) ppNewLine fs
>   ppDataDef (DDef Enum n ps cs fs) =
>       showString "enum " .
>       showString n .
>       ppSpace .
>       ppInContext 1 (ppDefsW ppDataCtrName (showChar ',' . ppNewLine) (map unL cs) . ppNewLine) .
>       ppNewLine .
>       ppDefsW (ppEquation' 1) ppNewLine fs

>   ppAlias :: AliasDefinition -> ShowS
>   ppAlias (ADef n ps t) = 
>       showString "type " .
>       showString n .
>       ppSpace .
>       ppDefs ppTypeParam ps .
>       showString "= " .
>       ppType t .
>       ppSemicolon  

>   ppStateP :: SType -> ShowS
>   ppStateP t =
>       showString ": " .
>       ppType t .
>       ppSpace

>   ppStateDef :: StateDefinition -> ShowS
>   ppStateDef s =
>       showString "state " .
>       showString (sDefName s) .
>       ppSpace .
>       ppDefs ppTypeParam (sDefParams s) .
>       ppMaybe ppStateP (sDefParent s) .
>       ppInContext 1 (ppStateDataFields (sDefCtrs s)) .
>       ppDefsW (ppEquation' 1) ppNewLine (sDefAcs s)

>   ppDecType :: DecType -> ShowS
>   ppDecType (DecTy ns t) =
>       ppNames ns .
>       ppTyping .
>       ppTypeS t .
>       ppSemicolon 

    {----------------------------------------------------------------------}
    {-- Expressions and Statements                                        -}
    {----------------------------------------------------------------------}

>   ppOption :: Int -> Option -> ShowS
>   ppOption n (Option p e) = 
>       ppIndent n . 
>       ppPattern p . 
>       showString " -> " . 
>       ppExpr n e 

>   ppOptions :: Int -> [Option] -> ShowS
>   ppOptions n [o]    = ppOption n o . ppNewLine
>   ppOptions n (o:os) = ppOption n o . ppNewLine . ppOptions n os

>   ppStatement :: Int -> Statement -> ShowS
>   ppStatement n (Statement e) =
>       ppIndent n .
>       ppExpr n e .
>       ppSemicolon 
>   ppStatement n (Bind p e) =
>       ppIndent n .
>       ppPattern p .
>       showString " <- " .
>       ppExpr n e .
>       ppSemicolon
>   ppStatement n (Getter p f) =
>       ppIndent n .
>       ppPattern p .
>       showString " <: " .
>       showString f .
>       ppSemicolon 
>   ppStatement n (Setter e f) =
>       ppIndent n .
>       ppExpr n e .
>       showString " >: " .
>       showString f .
>       ppSemicolon 

>   ppLiteral :: Literal -> ShowS
>   ppLiteral UnitLit      = showString "()"
>   ppLiteral (IntLit v)   = showString v
>   ppLiteral (StrLit v)   = showString v
>   ppLiteral (PairLit es) = ppInParens $ ppDefsW (ppExpr 0) (showChar ',') es
>   ppLiteral (ListLit es) = ppList $ ppDefsW (ppExpr 0) (showChar ',') es

>   ppExpr' :: Bool -> Int -> Expr -> ShowS
>   ppExpr' _ n (Var x)      = showString x
>   ppExpr' _ n (Ctr x)      = showString x
>   ppExpr' _ _ (Lit l)      = ppLiteral l
>   ppExpr' w n (Abs (Alt ps e)) = ppInOptParens w $ 
>       showChar '\\' . 
>       ppPatterns ps . 
>       showString " -> " . 
>       ppExpr n e
>   ppExpr' w n (App l r)    = ppInOptParens w $ 
>       ppExpr n l . 
>       ppSpace . 
>       ppExpr' True n r
>   ppExpr' w n (InfixOp o l r) = ppInOptParens w $ 
>       ppExpr n l .
>       ppSpace .
>       showString o .
>       ppSpace . 
>       ppExpr n r 
>   ppExpr' _ n (Cond c l r) = 
>       showString "if " . 
>       ppExpr n c . 
>       showString " then " . 
>       ppExpr n l . 
>       showString " else " . 
>       ppExpr n r
>   ppExpr' _ n (Let as e) = 
>       showString "let " . 
>       ppDefsW (ppAlt n) ppNewLine as .
>       showString " in " . 
>       ppExpr n e
>   ppExpr' w n (Case e os)  = ppInOptParens w $ 
>       showString "case " . 
>       ppExpr (n+1) e . 
>       ppSpace . 
>       ppInContext n (ppOptions (n+1) os)
>   ppExpr' _ n (Do stmts)   = ppInContext n $ ppDefsW (ppStatement (n+1)) ppNewLine stmts . ppNewLine

>   ppExpr :: Int -> Expr -> ShowS
>   ppExpr = ppExpr' False

>   ppAlt :: Int -> Alt -> ShowS
>   ppAlt i (Alt ps e) =
>       ppPatterns ps .
>       showString " = " .
>       ppExpr i e

>   ppEquation' :: Int -> Equation -> ShowS
>   ppEquation' i (Eq n alt) =
>       ppIndent i .
>       showString n .
>       ppSpace .
>       ppAlt i alt

>   ppEquation :: Equation -> ShowS
>   ppEquation = ppEquation' 0

>   ppModule :: LocP Module -> ShowS
>   ppModule (Loc (Module n ds) p) = 
>       ppPosn p .
>       showString " module " . 
>       showString n . 
>       ppSpace . 
>       ppInContext 0 (ppDefs ppLDef ds)

>   ppLDef :: LocP Definition -> ShowS
>   ppLDef (Loc x p) =
>       ppTab .
>       ppPosn p .
>       ppSpace .
>       ppDef x .
>       ppNewLine

>   ppDef :: Definition Loc -> ShowS
>   ppDef (ImportDef m) = 
>       showString "import " .
>       showString m .
>       ppSemicolon 
>   ppDef (TypeDef d)  = ppAlias d
>   ppDef (TyClDef d)  = ppTypeClass d 
>   ppDef (InstDef d)  = ppInstance d
>   ppDef (DataDef d)  = ppDataDef d 
>   ppDef (StateDef d) = ppStateDef d
>   ppDef (TypeDec t)  = ppDecType t
>   ppDef (ValueDef e) = ppEquation e
    
{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
