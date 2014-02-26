{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

This module contains Cada's surface abstract syntax tree. In other words, it contains all the data
types which are constructed by the parser from a Cada source file.

> module Cada.AST (

    {- Module re-exports -}

>   module Cada.Location,
>   module Cada.Pattern,
>   module Cada.STypes,
>   module Cada.Typed,

>   AlexPosn(..),
>   FreeVars(..),
>   ModuleName(..),
>   Positioned(..),
>   Pattern(..),

    {- Types -}

>   SType(..),

    {- SType classes and instances -}

>   STypeClass(..),
>   InstDef(..),
>   SInstance(..),
>   instHeadType,

>   TypeConstraint(..),
>   TypeParam(..),
>   Module(..),
>   DataField(..),
>   DataConstructor(..),
>   DataDefType(..),
>   DataDefinition(..),
>   AliasDefinition(..),
>   StateDataField(..),
>   StateDefinition(..),
>   Option(..),
>   Statement(..),
>   Literal(..),
>   Expr(..),
>   Alt(..),
>   Equation(..),
>   DecType(..),
>   Definition(..),

>   Relation(..),
>   TaggedRule(..),
>   TaggedDefinition(..)

> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import qualified Data.Set as S
    
>   import Cada.Lexer
>   import Cada.Name
>   import Cada.Location
>   import Cada.Pattern
>   import Cada.STypes
>   import Cada.Typed

>   import TypeSystem.Kind
 
>   (\\) = (S.\\)
 
    {----------------------------------------------------------------------}
    {-- SType Classes                                                      -}
    {----------------------------------------------------------------------}
 
>   class FreeVars a where
>       freeVars :: a -> S.Set String

>   class Binding a where
>       boundIn :: a -> S.Set String

>   instance (FreeVars a) => FreeVars [a] where
>       freeVars = foldr S.union S.empty . map freeVars
 
>   instance (Binding a) => Binding [a] where
>       boundIn = foldr S.union S.empty . map boundIn
 
    {----------------------------------------------------------------------}
    {-- Abstract Syntax Tree                                              -}
    {----------------------------------------------------------------------}

>   type ModuleName = String
    
    A module has a name and consists of a set of definitions.
    
>   data Module a = Module {
>       moduleName :: ModuleName,
>       moduleDefs :: [RecA a Definition]
>   }

    {----------------------------------------------------------------------}
    {-- Patterns                                                          -}
    {----------------------------------------------------------------------}

>   data Pattern = VarPattern {
>       varPatName :: String
>   }            | LitPattern {
>       litPatVal :: Literal
>   }            | CtrPattern {
>       ctrPtrName :: String,
>       ctrPtrArgs :: [Pattern]
>   }            | Wildcard 

>   instance FreeVars Pattern where
>       freeVars (VarPattern x)    = S.singleton x
>       freeVars (LitPattern l)    = freeVars l
>       freeVars (CtrPattern _ ps) = freeVars ps
>       freeVars _                 = S.empty

>   instance Binding Pattern where
>       boundIn (VarPattern x)     = S.singleton x
>       boundIn (LitPattern l)     = boundIn l
>       boundIn (CtrPattern _ ps)  = boundIn ps
>       boundIn _                  = S.empty

    {----------------------------------------------------------------------}
    {-- SType classes and instances                                       -}
    {----------------------------------------------------------------------}

>   data STypeClass a = TyClass {
>       tyClConstrs :: [TypeConstraint],
>       tyClName    :: String,
>       tyClParams  :: [String],
>       tyClDefs    :: [a DecType]
>   }

    We allow explicit type annotations in type class instances.

>   data InstDef a = InstTyDef {
>       instTyDef  :: a DecType
>   }              | InstValDef {
>       instValDef :: Equation
>   }

>   data SInstance a = SInst {
>       instConstrs :: [TypeConstraint],
>       instClass   :: String,
>       instParam   :: SType,
>       instBody    :: [InstDef a]
>   }

>   instHeadType :: SInstance a -> String
>   instHeadType inst = case sTypeDomain (instParam inst) of
>       (STyCtr n)   -> n
>       (STyTuple _) -> "(,)"
>       (STyList _)  -> "[]"

    {----------------------------------------------------------------------}
    {-- Data types                                                        -}
    {----------------------------------------------------------------------}

>   data DataField = DField {
>       dFieldName :: String,
>       dFieldType :: SType,
>       dFieldPos  :: AlexPosn
>   }

>   instance Eq DataField where
>       (DField x _ _) == (DField y _ _) = x == y

>   instance Positioned DataField where
>       srcPos = FilePos . dFieldPos

>   instance FreeTyVars DataField where
>       freeTyVars (DField _ t _) = freeTyVars t

>   data DataConstructor = DCtr {
>       dCtrName   :: String,
>       dCtrFields :: [DataField]
>   }

>   instance Eq DataConstructor where
>       (DCtr x _) == (DCtr y _) = x == y

>   instance FreeTyVars DataConstructor where
>       freeTyVars (DCtr _ fs) = freeTyVars fs

>   data DataDefType = Data | Single | Enum | Newtype | State

>   data DataDefinition = DDef {
>       dDefType   :: DataDefType,
>       dDefName   :: String,
>       dDefParams :: [TypeParam],
>       dDefCtrs   :: [Loc (Typed DataConstructor)],
>       dDefProjs  :: [Equation]
>   }

>   data AliasDefinition = ADef {
>       aDefName   :: TypeName,
>       aDefParams :: [TypeParam],
>       aDefType   :: SType
>   }

>   data StateDataField = SField {
>       sFieldName :: String,
>       sFieldVal  :: Expr,
>       sFieldType :: SType,
>       sFieldPos  :: AlexPosn
>   }

>   data StateDefinition = SDef {
>       sDefName   :: String,
>       sDefParams :: [TypeParam],
>       sDefParent :: Maybe SType,
>       sDefCtrs   :: [StateDataField],
>       sDefType   :: AliasDefinition,
>       sDefData   :: DataDefinition,
>       sDefAcs    :: [Equation]
>   }

>   data DecType = DecTy {
>       defSigNames   :: [String],
>       defSigType    :: TyScheme
>   }

>   data Relation = Rel {
>       relLHS :: [Loc String],
>       relRHS :: [Loc String]
>   }

>   data TaggedRule = TagRule {
>       tRulePatterns :: [Pattern],
>       tRuleType     :: TyScheme
>   }

>   data TaggedDefinition = TagDef {
>       tDefCtr    :: String,
>       tDefParams :: [TypeParam],
>       tDefRels   :: [Relation],
>       tDefRules  :: [TaggedRule]
>   }

    {----------------------------------------------------------------------}
    {-- Expressions and Statements                                        -}
    {----------------------------------------------------------------------}

>   data Option = Option {
>       optPattern :: Pattern,
>       optExpr    :: Expr
>   }

>   instance FreeVars Option where
>       freeVars (Option p e) = freeVars e \\ freeVars p

>   data Statement = Statement {
>       stmtExpr   :: Expr
>   }              | Bind {
>       bindPat    :: Pattern,
>       bindExpr   :: Expr
>   }              | Getter {
>       getterPat  :: Pattern,
>       getterName :: String
>   }              | Setter {
>       setterExpr :: Expr,
>       setterName :: String
>   } 

>   instance FreeVars Statement where
>       freeVars (Statement e) = freeVars e
>       freeVars (Bind p e)    = freeVars e \\ freeVars p
>       freeVars (Getter p s)  = S.singleton (s ++ ".get")
>       freeVars (Setter e s)  = freeVars e `S.union` S.singleton (s ++ ".set")

>   data Literal = UnitLit
>                | IntLit {
>       intLitVal :: String
>   }            | StrLit {
>       strLitVal :: String
>   }            | PairLit {
>       pairVals  :: [Expr] 
>   }            | ListLit {
>       listLitVals :: [Expr]
>   }

>   instance FreeVars Literal where
>       freeVars (PairLit es) = freeVars es 
>       freeVars (ListLit es) = freeVars es
>       freeVars _            = S.empty

>   instance Binding Literal where
>       boundIn (PairLit es) = S.unions (map boundIn es)
>       boundIn (ListLit es) = S.unions (map boundIn es)
>       boundIn _            = S.empty

    A typed expression is an expression with an explicit type annotation.

>   data Expr = Var {
>       varName :: String
>   }         | Ctr {
>       ctrName :: String
>   }         | Lit {
>       litValue :: Literal
>   }         | Abs {
>       absAlt   :: Alt
>   }         | App {
>       appLeft    :: Expr,
>       appRight   :: Expr
>   }         | InfixOp {
>       inopName   :: String,
>       inopLeft   :: Expr,
>       inopRight  :: Expr
>   }         | Let {
>       letAlts    :: [Alt],
>       letExpr    :: Expr
>   }         | Cond {
>       condExpr   :: Expr,
>       condTrue   :: Expr,
>       condFalse  :: Expr
>   }         | Case {
>       caseExpr   :: Expr,
>       caseOptns  :: [Option]
>   }         | Do {
>       doStmts    :: [Statement]
>   }         

    The type inference algorithm may refine types more than the Haskell type inference allows,
    so we need to generate `unsafeCoerce' in the right places.

>             | Cast {
>       castExpr   :: Expr 
>   }

>   instance FreeVars Expr where
>       freeVars (Var x)    = S.singleton x
>       freeVars (Ctr x)    = S.singleton x
>       freeVars (Lit l)    = freeVars l
>       freeVars (Abs alt)  = freeVars alt
>       freeVars (App f a)  = freeVars f `S.union` freeVars a
>       freeVars (InfixOp n l r) = S.singleton n `S.union` freeVars l `S.union` freeVars r
>       freeVars (Let as e)      = freeVars as `S.union` (freeVars e \\ boundIn as) 
>       freeVars (Cond c t f)    = freeVars c `S.union` freeVars t `S.union` freeVars f
>       freeVars (Case e os)     = freeVars e `S.union` freeVars os
>       freeVars (Do stmts)      = freeVars stmts

>   instance Binding Expr where
>       boundIn (Var x) = S.singleton x
>       boundIn (Lit l) = boundIn l
>       boundIn _       = error "Expr.boundIn"

>   data Alt = Alt {
>       altPats :: [Pattern],
>       altExpr :: Expr
>   }

>   instance FreeVars Alt where
>       freeVars (Alt ps e) = freeVars e \\ freeVars ps

>   instance Binding Alt where
>       boundIn (Alt ps _) = boundIn ps

>   data Equation = Eq {
>       eqName :: String,
>       eqAlt  :: Alt
>   }

>   instance FreeVars Equation where
>       freeVars (Eq _ alt) = freeVars alt

    {----------------------------------------------------------------------}
    {-- Definitions                                                       -}
    {----------------------------------------------------------------------}

    A Cada module consists of definitions for various things.

>   data Definition a = ImportDef {
>       iDefModule :: ModuleName
>   }               | TypeDef {
>       tDefVal    :: AliasDefinition
>   }               | TyClDef {
>       tClDef     :: STypeClass a
>   }               | InstDef {
>       instDef    :: SInstance a
>   }               | DataDef {
>       dDefVal    :: DataDefinition
>   }               | StateDef {
>       sDefVal    :: StateDefinition
>   }               | TypeDec {
>       tdec       :: DecType
>   }               | ValueDef {
>       vdecEq     :: Equation
>   }               | TaggedDef {
>       tagDefVal  :: TaggedDefinition
>   }

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
