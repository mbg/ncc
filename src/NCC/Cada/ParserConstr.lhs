{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module Cada.ParserConstr (
>   makeCtrPattern,
>   exprToPattern,
>   exprToField,
>   makeContext,
>   makeTyParam,
>   makeTyVar,
>   makeTyConstr,
>   makeDefSig,
>   makeTypeClass,
>   makeInstance,
>   makeDataCtrL,
>   makeField,
>   makeSField,
>   makeEnum,
>   makeDataS,
>   makeDataN,
>   makeData,
>   makeState,
>   makeBind,
>   makeSetter,
>   makeSetter',
>   makeGetter,
>   checkStmts,
>   makeCond,
>   makeLet,
>   makeParensExpr,
>   makeEquation,
>   makeValDef,
>   makeModule,
>   makeImportDef,
>   makeTypeDef,
>   makeTyClDef,
>   makeInstDef,
>   makeTypeDec
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Control.Applicative
    
>   import Data.Char (toLower)
>   import Data.List ((\\))
>   import Data.Foldable (foldrM)
    
>   import Cada.Lexer (TokenP, tVal, tkVal)
>   import Cada.Location
>   import Cada.AST
>   import Cada.ParserMonad
>   import Cada.ParserError

>   import TypeSystem.Kind (Kind(..))
 
    {----------------------------------------------------------------------}
    {-- Validation & Construction Functions for Patterns                  -}
    {----------------------------------------------------------------------}
 
>   makeCtrPattern :: [Pattern] -> Pattern
>   makeCtrPattern []  = LitPattern UnitLit
>   makeCtrPattern [e] = e
>   makeCtrPattern ps  
>       | length ps == 2 = CtrPattern "(,)" ps
>       | otherwise      = error "Only pairs supported!"

    In some cases we need to parse patterns as expressions in order to avoid
    ambiguities. This function converts such an expression to a pattern.

>   exprToPattern :: Expr -> Parser Pattern
>   exprToPattern (Var n)   = return $ VarPattern n
>   exprToPattern (Ctr n)   = return $ CtrPattern n []
>   exprToPattern (Lit l)   = return $ LitPattern l
>   exprToPattern (App f a) = do
>       fp <- exprToPattern f
>       case fp of
>           (CtrPattern n ps) -> do
>               ap <- exprToPattern a
>               return $ CtrPattern n (ps ++ [ap])
>           otherwise         -> error "exprToPattern1"
>   exprToPattern (InfixOp ":" l r) = do
>       l' <- exprToPattern l
>       r' <- exprToPattern r
>       return $ CtrPattern ":" [l',r']
>   exprToPattern _         = error "exprToPattern2"

>   exprToField :: Expr -> Parser String
>   exprToField (Var n) = return n
>   exprToField _       = error "exprToField expected something else"

    {----------------------------------------------------------------------}
    {-- Validation & Construction Functions for Types                     -}
    {----------------------------------------------------------------------}

>   makeContext' :: SType -> Parser TypeConstraint
>   makeContext' (STyApp (STyCtr c) (STyVar x)) = return $ TyConstr c x
>   makeContext' t                              = error "Type constraint must be of form T a"
    
>   makeContext :: SType -> Parser [TypeConstraint]
>   makeContext (STyTuple ts) = mapM makeContext' ts
>   makeContext t             = sequence [makeContext' t]
    
    Constructs an (optionally) indexed TypeParam from a value of type 
    TokenP: (p,t).
    
    Assumptions:
        None.
        
    Error conditions:
        1. The value of `t' must be an unqualified identifier (qualErrH).

>   makeTyParam :: TokenP -> Parser TypeParam
>   makeTyParam (p,t) = do
>       qualErrH (FilePos p) (tVal t) 
>       return $ TyP (tVal t) p
    
    Constructs a (surface) type variable from a value of type TokenP: (p,t).
    
    Assumptions:
        None.
        
    Error conditions:
        1. The value of `t' must be an unqualified identifier (qualErrH).

>   makeTyVar :: TokenP -> [SType] -> Parser SType
>   makeTyVar (p,t) as = do
>       qualErrH (FilePos p) (tVal t) 
>       return $ STyVar $ tVal t

    Constructs a (surface) type class constraint from two values of
    type TokenP, one represents the name of the type class and the
    other one represents the name of the type variable on which
    the constraint should be placed.
    
    Assumptions:
        None.
        
    Error conditions:
        1. The value of `tn` must be an unqualified identifier (qualErrH).
        2. The value of `vn` must be an unqualified identifier (qualErrH).

>   makeTyConstr :: TokenP -> TokenP -> Parser TypeConstraint
>   makeTyConstr (p,tn) (p',vn) = do
>       qualErrH (FilePos p) (tVal tn)
>       qualErrH (FilePos p') (tVal vn)
>       return $ TyConstr (tVal tn) (tVal vn)
    
    Constructs a type declaration for a given list of names using a context
    and surface type.
    
    Assumptions:
        1. Values of type TypeConstraint should be valid
    
    Error conditions:
        None.
    
>   makeDefSig :: 
>       Loc [String]     -> 
>       [TypeConstraint] -> 
>       SType            -> 
>       Parser (Loc DecType)
>   makeDefSig lns cs t = do
>       return $ (\ns -> DecTy ns (quantify (cs :==> t))) <$> lns

    {----------------------------------------------------------------------}
    {-- Type Classes and Instances                                        -}
    {----------------------------------------------------------------------}        
    
>   reduceTypeS :: [String] -> TyScheme -> TyScheme
>   reduceTypeS vs (Scheme vs' qt) = Scheme (vs' \\ vs) qt
    
>   fixClassTypeS :: [String] -> Loc DecType -> Loc DecType
>   fixClassTypeS ps (Loc (DecTy ns ts) p) = Loc (DecTy ns ts') p
>       where
>           ts' = reduceTypeS ps ts
    
>   typeToHead :: SType -> Parser (String, [String])
>   typeToHead (STyCtr n)   = return (n, [])
>   typeToHead (STyVar n)   = return ("", [n])
>   typeToHead (STyApp f a) = do
>       (n, ps) <- typeToHead f
>       if null n then error "type class head not in hnf"
>       else do
>           (n', ps') <- typeToHead a
>           if null n' then return (n, ps++ps')
>           else error "head must be of form C x x1 ... xn"
    
    Constructs a type class

    Assumptions:
        
        
    Error conditions:
        
        
        
    
>   makeTypeClass :: 
>       [TypeConstraint] -> 
>       SType            -> 
>       [Loc DecType]    -> 
>       Parser (STypeClass Loc)
>   makeTypeClass cs t fs = do
>       (n,ps) <- typeToHead t -- check that `t' is in head normal form
>       -- check duplicate constraints?
>       --duplicatesCheckH ppTypeParamError ps
>       return $ TyClass cs n ps $ map (fixClassTypeS ps) fs

    Constructs a class instance if given a context (a list of super classes),
    a value of type TokenP which represents the name of the class, a surface
    type which should be made an instance of the class and a list of instance
    declarations.
    
    Assumptions:
        1. The type constraints are valid
        2. 

>   splitClassHead :: SType -> Parser (String, SType)
>   splitClassHead (STyApp (STyCtr n) r) = do return (n, r)
>   splitClassHead _                     = do error "invalid class head"
        
>   makeInstance ::
>       [TypeConstraint] ->
>       SType            ->
>       [InstDef Loc]    ->
>       Parser (SInstance Loc)
>   makeInstance cs t ds = do
>       (cls, st) <- splitClassHead t
>       checkInstHead UnknownPos cls st
>       return $ SInst cs cls st ds 
    
    {----------------------------------------------------------------------}
    {-- Data Types                                                        -}
    {----------------------------------------------------------------------}
        
    Constructs a DataConstructor from a TokenP `(p,t)' and a list of data
    fields `fs'. It then adds the data constructor to a list of data
    constructors `cs'.
    
    Assumptions:
        None.
        
    Error conditions:
        1. The value of `t' must be an unqualified identifier (qualErrH).
        2. All fields in `fs' must have unique names (duplicatesCheckH).

>   makeDataCtrL :: 
>       TokenP                -> 
>       [DataField]           -> 
>       [Loc DataConstructor] -> 
>       Parser [Loc DataConstructor]
>   makeDataCtrL (p,tk) fs cs = let 
>           n = (tVal tk) in do
>       qualErrH (FilePos p) n
>       duplicatesCheckH (ppDupFieldError n . dFieldName) fs
>       return $ inL p (DCtr n fs) : cs
    
    Constructs a DataField from a TokenP `(p,n)' with a SType 't'.
    
    Assumptions:
        None.
        
    Error conditions:
        1. The value of `n' must be an unqualified identifier (qualErrH).
    
>   makeField :: TokenP -> SType -> Parser DataField
>   makeField (p,n) t = do
>       qualErrH (FilePos p) (tVal n)
>       return $ DField (tVal n) t p

>   makeSField :: TokenP -> Expr -> SType -> Parser StateDataField
>   makeSField (p,n) e t = do
>       qualErrH (FilePos p) (tVal n)
>       return $ SField (tVal n) e t p
    
    Constructs a DataDefinition from a String `n' and a list of data
    constructors `cs'.
    
    Assumptions:
        1. All data constructors have valid fields.
        
    Error conditions:
        1. All constructors in `cs' must have unique names (duplicatesCheckH).

>   makeDataTy :: String -> [TypeParam] -> SType
>   makeDataTy n = foldl STyApp (STyCtr n) . map (STyVar . tyParamName)

>   makeCtrType :: SType -> Loc DataConstructor -> Loc (Typed DataConstructor)
>   makeCtrType pt (Loc dc p) = let fs = dCtrFields dc in
>       Loc (Ty dc $ Scheme [] $ [] :==> foldr tyArrow pt (map dFieldType fs)) p

>   mkCtrPat' :: String -> [DataField] -> Pattern
>   mkCtrPat' n fs = CtrPattern n $ map fst $ zip [VarPattern ('x' : show n) | n <- [0..]] fs

>   mkCtrPat :: DataConstructor -> Pattern
>   mkCtrPat (DCtr n fs) = mkCtrPat' n fs

>   makeProjFun :: Pattern -> (Int,DataField) -> Accum Equation
>   makeProjFun p (i, DField n st _) = consA $ Eq n $ Alt [p] $ Var ('x' : show i)

>   makeProjFuns :: Pattern -> DataConstructor -> Accum Equation
>   makeProjFuns p (DCtr n fs) eqs = foldr (makeProjFun p) eqs (zip [0..] fs)

>   makeFields :: [Loc DataConstructor] -> Accum Equation
>   makeFields []     = id
>   makeFields (x:xs) = makeProjFuns (mkCtrPat (unL x)) (unL x) . makeFields xs

>   makeDataDef :: 
>       DataDefType           ->
>       String                -> 
>       [TypeParam]           -> 
>       [Loc DataConstructor] -> 
>       Parser DataDefinition
>   makeDataDef t n ps cs = let 
>           dt = makeDataTy n ps 
>           tc = map (makeCtrType dt) cs 
>           fs = makeFields cs in do
>       duplicatesCheckH' (ppDupCtrError n . dCtrName) cs
>       return $ DDef t n ps tc (unA fs)

    Constructs a Definition for an enumeration at position `p' from TokenP
    `t' and data constructors 'cs'.
    
    Assumptions:
        1. All data constructors `cs' have valid fields.
        
    Error conditions:
        1. The value of `t' must be an unqualified identifier (qualErrH).
        2. All constructors in `cs' must have unique names (makeDataDef).
    
>   makeEnum :: 
>       AlexPosn              -> 
>       TokenP                -> 
>       [Loc DataConstructor] -> 
>       Parser (LocP Definition)
>   makeEnum p (p',t) cs = do
>       qualErrH (FilePos p') (tVal t)
>       d <- makeDataDef Enum (tVal t) [] cs
>       returnL p $ DataDef d

    Constructs a Definition for a single-constructor data type at position
    `p' from TokenP `t', type parameters `ps' and data fields `fs'.
    
    Assumptions:
        1. All data fields `fs' have valid names.
        
    Error conditions:
        1. The value of `t' must be an unqualified identifier (qualErrH).
        2. All type parameters `ps' must have unique names (duplicatesCheckH).
        3. All data fields `fs' must have unique names (duplicatesCheckH).
        4. All free type variables in `fs' are in `ps' (dctrFreeVarErrH).
    
>   buildDataCtr :: 
>       AlexPosn ->
>       String -> 
>       [DataField] -> 
>       Parser (Loc DataConstructor)
>   buildDataCtr p n fs = returnL p $ DCtr n fs
    
>   makeDataS :: AlexPosn -> TokenP -> [TypeParam] -> [DataField] -> Parser (LocP Definition)
>   makeDataS p (p',tv) ps fs = let n = tVal tv in do
>       qualErrH (FilePos p') n
>       duplicatesCheckH (ppTypeParamError . tyParamName) ps
>       duplicatesCheckH (ppDupFieldErrorS n . dFieldName) fs
>       c <- buildDataCtr p' n fs
>       dctrFreeVarErrH n ps [c]
>       d <- makeDataDef Single n ps [c]
>       returnL p $ DataDef d

    Constructs a Definition for a newtype at position `p' from TokenP `nt', 
    type parameters `ps' and a single data constructor whose name is taken
    from `ct' and which has a single field `df'.
    
    Assumptions:
        
    Error conditions:
        1. The value of `nt' must be an unqualified identifier (qualErrH).
        2. All type parameters `ps' must have unique names (duplicatesCheckH).
        3. All free type variables in `df' are in `ps' (dctrFreeVarErrH).

>   makeDataN :: 
>       AlexPosn -> 
>       TokenP -> 
>       [TypeParam] -> 
>       TokenP -> 
>       DataField ->
>       Parser (LocP Definition)
>   makeDataN p nt ps ct df = let
>           nv = tkVal nt
>           np = fst nt
>           cv = tkVal ct
>           cp = fst ct in do
>       qualErrH (FilePos np) nv  
>       duplicatesCheckH (ppTypeParamError . tyParamName) ps  
>       c <- buildDataCtr cp cv [df] 
>       dctrFreeVarErrH nv ps [c]
>       d <- makeDataDef Newtype nv ps [c]
>       returnL p $ DataDef d  

    Constructs a Definition for a data type at position `p' from TokenP `t', 
    type parameters `ps' and data constructors `ds'.
    
    Assumptions:
        1. All data constructors have valid fields.
        
    Error conditions:
        1. The value of `t' must be an unqualified identifier (qualErrH).
        2. All type parameters `ps' must have unique names (duplicatesCheckH).
        3. All constructors in `cs' must have unique names (makeDataDef).
        4. All free type variables in `cs' are in `ps' (dctrFreeVarErrH).
    
>   makeData :: 
>       AlexPosn              ->
>       TokenP                -> 
>       [TypeParam]           -> 
>       [Loc DataConstructor] ->
>       Parser (LocP Definition)
>   makeData p (p',tv) ps cs = let n = tVal tv in do
>       qualErrH (FilePos p') n
>       duplicatesCheckH (ppTypeParamError . tyParamName) ps
>       dctrFreeVarErrH n ps cs
>       d <- makeDataDef Data n ps cs
>       returnL p $ DataDef d

    {----------------------------------------------------------------------}
    {-- State Types                                                       -}
    {----------------------------------------------------------------------}
    
>   makeTyVarsForParams :: [TypeParam] -> [SType]
>   makeTyVarsForParams = map (STyVar . tyParamName)

>   makeStateDataTy :: String -> [TypeParam] -> SType
>   makeStateDataTy dn = foldl STyApp (STyCtr dn) . makeTyVarsForParams
    
>   makeStateTyRHS :: Maybe SType -> SType -> SType
>   makeStateTyRHS Nothing  dt = STyApp (STyCtr "State") dt
>   makeStateTyRHS (Just t) dt = STyApp (STyApp (STyCtr "StateT") dt) t
    
>   makeStateTy :: 
>       AlexPosn    ->
>       Maybe SType ->
>       String      -> 
>       String      ->
>       [TypeParam] -> 
>       Parser AliasDefinition
>   makeStateTy p mp tn dn ps = let
>           dt = makeStateDataTy dn ps in 
>       return $ ADef tn ps $ makeStateTyRHS mp dt

>   modifyCtrVal :: Pattern -> Int -> String -> Expr
>   modifyCtrVal (CtrPattern c es) i n = foldl App (Ctr c) $ 
>       map (\(VarPattern v,j) -> if i == j then App (Var n) (Var v) else Var v) ps -- then Var n else Var v
>       where
>           ps = zip es [0..]
>           

>   mkGetter :: String -> Equation
>   mkGetter n = Eq (n ++ ".get") $ Alt [] $ App (Var "gets") (Var n)

>   mkSetter :: String -> Pattern -> Int -> Equation
>   mkSetter n p i = Eq (n ++ ".set") $ Alt [VarPattern "v"] $ App (Var "modify") (App (Var (n ++ ".update")) (App (Var "const") (Var "v"))) --(Abs $ Alt [p] (modifyCtrVal p i "v"))

>   mkUpdate :: String -> Pattern -> Int -> Equation
>   mkUpdate n p i = Eq (n ++ ".update") $ Alt [VarPattern "f"] $ (Abs $ Alt [p] (modifyCtrVal p i "f"))

>   mkModify :: String -> Equation
>   mkModify n = Eq (n ++ ".modify") $ Alt [VarPattern "f"] $ Do [Bind (VarPattern "v") (Var (n ++ ".get")), Statement (App (Var (n ++ ".set")) (App (Var "f") (Var "v")))]

>   makeAccessor :: Pattern -> StateDataField -> (Int,[Equation]) -> (Int,[Equation])
>   makeAccessor p (SField n e _ _) (i,eqs) = (i-1, get : set : upd : mod : eqs)
>       where
>           get = mkGetter n
>           set = mkSetter n p i
>           upd = mkUpdate n p i
>           mod = mkModify n

>   makeAccessors :: Pattern -> [StateDataField] -> [Equation]
>   makeAccessors pat fs = snd $ foldr (makeAccessor pat) (length fs - 1,[]) fs

>   mkSCtrPat :: String -> [StateDataField] -> Pattern
>   mkSCtrPat n fs = CtrPattern n $ map fst $ zip [VarPattern ('x' : show n) | n <- [0..]] fs

>   makeStateDefault :: String -> [StateDataField] -> Equation
>   makeStateDefault n fs = Eq (map toLower n ++ ".default") $ Alt [] $ foldl App (Var n) (map sFieldVal fs)

>   stToDField :: StateDataField -> DataField
>   stToDField (SField n _ t p) = DField n t p

>   makeState ::
>       AlexPosn    ->
>       TokenP      ->
>       [TypeParam] ->
>       Maybe SType ->
>       [StateDataField] ->
>       Parser (LocP Definition)
>   makeState p tp ps mt fs = let 
>           tn = tkVal tp 
>           dn = tn ++ "Data"
>           as = makeAccessors (mkSCtrPat tn fs) fs 
>           ds = makeStateDefault tn fs in do
>       c  <- buildDataCtr p tn (map stToDField fs)
>       d  <- makeDataDef State dn ps [c]
>       t  <- makeStateTy p mt tn dn ps
>       returnL p $ StateDef $ SDef tn ps mt fs t d (ds : as)

    {----------------------------------------------------------------------}
    {-- Expressions                                                       -}
    {----------------------------------------------------------------------}

>   makeBind :: Expr -> Expr -> Parser Statement
>   makeBind ep e = do
>       p <- exprToPattern ep
>       return $ Bind p e

>   makeGetter :: Expr -> TokenP -> Parser Statement
>   makeGetter ep tp = do
>       p <- exprToPattern ep
>       return $ Getter p (tkVal tp)

>   makeSetter' :: Expr -> TokenP -> Parser Statement
>   makeSetter' e tp = do
>       return $ Setter e (tkVal tp)

>   makeSetter :: Expr -> Expr -> Parser Statement
>   makeSetter e fe = do
>       f <- exprToField fe
>       return $ Setter e f

>   checkStmts :: [Statement] -> Parser Expr
>   checkStmts [] = error "list of statements can't be empty"
>   checkStmts xs = case last xs of
>       (Statement _) -> return $ Do xs
>       (Setter _ _)  -> return $ Do xs
>       otherwise     -> error "last statement must be an expression or a setter"
    
>   makeCond :: Expr -> Expr -> Expr -> Parser Expr
>   makeCond c t f = do
>       return $ Cond c t f

>   makeLet :: AlexPosn -> [Alt] -> Expr -> Parser Expr
>   makeLet p as e = do
>       return $ Let as e

>   makeParensExpr :: [Expr] -> Expr
>   makeParensExpr []  = Lit $ UnitLit
>   makeParensExpr [e] = e
>   makeParensExpr es  
>       | length es == 2 = Lit $ PairLit es
>       | otherwise      = error "Only pairs supported!"
    
   makeEqDecl :: TokenP -> [Pattern] -> Expr -> Parser DefDeclaration
   makeEqDecl (p,n) ps e = do
       qualErrH p (tVal n)
       return $ EqDecl (tVal n) ps e p
    
   makeDef :: TokenP -> Maybe DefType -> [DefDeclaration] -> Parser Definition
   makeDef (p,n) t ds = do
       equationErrH p (tVal n) ds
       return $ Def (tVal n) t ds p

    {----------------------------------------------------------------------}
    {-- Definitions                                                       -}
    {----------------------------------------------------------------------}
    
>   makeEquation :: 
>       Loc String ->
>       [Pattern]  ->
>       Expr       ->
>       Parser (Loc Equation)
>   makeEquation (Loc n p) ps e = do
>       return $ Loc (Eq n $ Alt ps e) p
    
>   makeValDef :: 
>       Loc Equation ->
>       Parser (LocP Definition)
>   makeValDef le = do
>       return $ ValueDef <$> le

    {----------------------------------------------------------------------}
    {-- Modules                                                           -}
    {----------------------------------------------------------------------}
    
>   makeModule ::
>       AlexPosn          ->
>       TokenP            ->
>       [LocP Definition] ->
>       Parser (LocP Module)
>   makeModule p (_,t) ds = do
>       returnL p $ Module (tVal t) ds

>   makeImportDef ::
>       AlexPosn ->
>       TokenP   ->
>       Parser (LocP Definition)
>   makeImportDef p (_,t) = do
>       returnL p $ ImportDef (tVal t)

    makeTypeDef -----------------------------------------------------------
    
    Constructs a type alias.
    
    Assumptions:
        None.
        
    Error conditions checked:
        1. The name of the alias must be an unqualified identifier (qualErrH).
        2. All parameters must have unique names (duplicatesCheckH).
        3. There may not be any free type variables (typeFreeVarErr)
        4. The definition may not be recursive 

>   makeTypeDef :: 
>       AlexPosn     -> 
>       TokenP       -> 
>       [TypeParam]  -> 
>       SType        -> 
>       Parser (LocP Definition)
>   makeTypeDef p tp ps t = let n = tkVal tp in do
>       qualErrH (FilePos p) n 
>       duplicatesCheckH (ppTypeParamError . tyParamName) ps
>       typeFreeVarErr n ps t 
>       recAliasErrH (FilePos p) n t
>       returnL p $ TypeDef $ ADef n ps t

>   makeTyClDef ::
>       AlexPosn       ->
>       STypeClass Loc ->
>       Parser (LocP Definition)
>   makeTyClDef p tc = do
>       returnL p $ TyClDef tc

>   makeInstDef ::
>       AlexPosn      ->
>       SInstance Loc ->
>       Parser (LocP Definition)
>   makeInstDef p inst = do
>       returnL p $ InstDef inst

>   makeTypeDec :: Loc DecType -> LocP Definition
>   makeTypeDec = fmap TypeDec 
    
{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
