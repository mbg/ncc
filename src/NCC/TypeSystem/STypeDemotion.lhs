{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

Functions which demote surface types to expressions. (see A system of constructor classes: overloading and implicit higher-order polymorphism)

> module TypeSystem.STypeDemotion (
>   module TypeSystem.BindGroup,
>
>   demoteTypeS,
>   demoteInstance,
>   demoteModule
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Control.Applicative ((<$>))
    
>   import Data.Graph
>   import qualified Data.Set as S

>   import Utility.Accum
    
>   import Cada.AST

>   import TypeSystem.Kind
>   import TypeSystem.BindGroup

    {----------------------------------------------------------------------}
    {-- Combinators                                                       -}
    {----------------------------------------------------------------------}

>   (<+>) :: Expr -> Expr -> Expr
>   (<+>) = InfixOp "+"

>   (<|>) :: Expr -> Expr -> Expr
>   (<|>) = InfixOp "|" 

>   tupleCtr :: [SType] -> Expr
>   tupleCtr ts = Var $ "t(" ++ replicate (length ts - 1) ',' ++ ")"
    
    {----------------------------------------------------------------------}
    {-- Type Demotion                                                     -}
    {----------------------------------------------------------------------}

>   demoteType :: SType -> Expr
>   demoteType (STyVar x)    = Var x
>   demoteType (STyCtr "->") = Var "t->"
>   demoteType (STyCtr x)    = Var ('t':x)
>   demoteType (STyApp f a)  = App (demoteType f) (demoteType a)
>   demoteType (STyList t)   = App (Var "t[]") (demoteType t)
>   demoteType (STyTuple ts) = foldl App (tupleCtr ts) (map demoteType ts)
>   demoteType (STyAnn t _)  = demoteType t

>   demoteQType :: TyQual SType -> Expr
>   --demoteQType ([]  :==> t) = demoteType t
>   demoteQType (ctx :==> t) = demoteContext ctx <+> demoteType t

>   demoteTypeS :: TyScheme -> Expr
>   demoteTypeS (Scheme [] qt) = demoteQType qt
>   demoteTypeS (Scheme vs qt) = Abs $ Alt (map VarPattern vs) $ demoteQType qt

>   demoteParams :: [TypeParam] -> [Pattern]
>   demoteParams = map (VarPattern . tyParamName)

>   demoteConstraint :: TypeConstraint -> Expr
>   demoteConstraint (TyConstr c x) = App (Var (ty c)) (Var x)

>   demoteContext :: [TypeConstraint] -> Expr
>   demoteContext = foldr (<+>) (Var "constraint") . map demoteConstraint
    
    {----------------------------------------------------------------------}
    {-- Demotion                                                          -}
    {----------------------------------------------------------------------}
    
>   ty :: String -> String
>   ty = (:) 't'

>   eqn :: String -> Expr -> Equation
>   eqn n = Eq (ty n) . Alt []

>   lambda :: [TypeParam] -> Expr -> Expr
>   lambda ps = Abs . Alt (demoteParams ps)
    
>   isTyDef :: Definition Loc -> Bool
>   isTyDef (TypeDef {})  = True
>   isTyDef (TyClDef {})  = True
>   isTyDef (DataDef {})  = True
>   isTyDef (StateDef {}) = True
>   isTyDef _             = False
    
>   demoteDecType :: DecType -> Expr
>   demoteDecType (DecTy _ t) = demoteTypeS t
    
>   demoteDecTypes :: Expr -> [Loc DecType] -> Expr
>   demoteDecTypes e ds = foldl1 (<|>) $ map (demoteDecType . unL) ds ++ [e]
    
>   demoteTyClass :: STypeClass Loc -> Equation
>   demoteTyClass (TyClass ctx n [] ds) = eqn n $ demoteDecTypes (demoteContext ctx) ds 
>   demoteTyClass (TyClass ctx n ps ds) = eqn n $ Abs $ Alt (map VarPattern ps) $ demoteDecTypes (demoteContext ctx) ds 
    
>   demoteInstance :: SInstance Loc -> Expr
>   demoteInstance (SInst ctx cls st bs) = Abs $ Alt (VarPattern <$> vars) $ (App (Var (ty cls)) te <|> cte)
>       where
>           cte  = demoteContext ctx
>           te   = demoteType st
>           vars = S.toList $ freeTyVarsN (ctx :==> st)
    
>   demoteDataCtr :: Typed DataConstructor -> Expr
>   demoteDataCtr = demoteTypeS . typedType

>   demoteDataCtrs :: [Loc (Typed DataConstructor)] -> Expr
>   demoteDataCtrs = foldr1 (<+>) . map (demoteDataCtr . unL)
    
>   demoteDataTy :: DataDefinition -> Equation
>   demoteDataTy (DDef _ n [] cs _) = eqn n $ demoteDataCtrs cs
>   demoteDataTy (DDef _ n ps cs _) = eqn n $ lambda ps $ demoteDataCtrs cs

>   demoteAlias :: AliasDefinition -> Equation
>   demoteAlias (ADef n [] t) = eqn n $ demoteType t
>   demoteAlias (ADef n ps t) = eqn n $ lambda ps (demoteType t)
    
>   demoteState :: StateDefinition -> Accum Equation
>   demoteState s = 
>       consA (demoteAlias (sDefType s)) . 
>       consA (demoteDataTy (sDefData s))
    
>   demoteDef :: Definition Loc -> Accum Equation
>   demoteDef (TypeDef d)  = consA $ demoteAlias d
>   demoteDef (TyClDef d)  = consA $ demoteTyClass d
>   demoteDef (DataDef d)  = consA $ demoteDataTy d
>   demoteDef (StateDef d) = demoteState d
    
>   demoteDefs :: [LocP Definition] -> [Equation]
>   demoteDefs = unA . mapA demoteDef . filter isTyDef . map unL
    
>   demoteModule :: Module Loc -> BindGroup 
>   demoteModule = toBG . findDeps . demoteDefs . moduleDefs

    {----------------------------------------------------------------------}
    {-- Sorting                                                           -}
    {----------------------------------------------------------------------}
    
>   type KindNode = (Equation, String, [String])

>   toKindNode :: Equation -> KindNode
>   toKindNode (Eq n alt) = (Eq n alt, n, S.toList $ freeVars alt)

>   findDeps :: [Equation] -> [SCC KindNode]
>   findDeps = stronglyConnCompR . map toKindNode

>   toImpl :: SCC KindNode -> [Impl]
>   toImpl (AcyclicSCC (eq,n,_)) = [Impl n [eq]]
>   toImpl (CyclicSCC ks)        = map (\(eq,n,_) -> Impl n [eq]) ks

>   toBG :: [SCC KindNode] -> BindGroup
>   toBG ks = BG [] (toImpl <$> ks) []

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          

