{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module TypeSystem.Conversion (
>   mkEnvs
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Debug.Trace
    
>   import Control.Applicative
>   import Control.Monad.State
    
>   import Data.Foldable (foldrM)
>   import qualified Data.Map as M
>   import qualified Data.Set as S
>   import Data.Maybe (fromJust, isJust)

>   import Utility.PrettyPrint
>   import Utility.Errors

>   import Cada.Pos
>   import Cada.AST
>   import Cada.PrettyPrint (ppType) 
    
>   import TypeSystem.Kind
>   import TypeSystem.Types
>   import TypeSystem.KindDemotion
>   import TypeSystem.Substitution
>   import TypeSystem.PolyType
>   import TypeSystem.DataType
>   import TypeSystem.Alias
>   import TypeSystem.AliasSort
>   import TypeSystem.TypeClass
>   import TypeSystem.TypeClassSort
>   import TypeSystem.Instance
>   import TypeSystem.KindPromotion
>   import TypeSystem.Unify
>   import TypeSystem.KindInference 
>   import TypeSystem.Assump
>   import TypeSystem.Environments
>   import TypeSystem.EnvMerge
>   import TypeSystem.StateType

    {----------------------------------------------------------------------}
    {-- Conversion Monad                                                  -}
    {----------------------------------------------------------------------}
    
>   data ConvData = CD {
>       cdAssumps :: Assumps,
>       cdEnvs    :: Envs
>   }
    
>   type Conversion = StateT ConvData (Errors (Loc String))
>   type EnvsTrans  = StateT Envs Conversion

>   failConv :: Pos -> ShowS -> Conversion a
>   failConv p = lift . addFatalError . inP p . ppDo

>   failTrans :: Pos -> ShowS -> EnvsTrans a
>   failTrans p = lift . failConv p

    {----------------------------------------------------------------------}
    {-- Errors                                                            -}
    {----------------------------------------------------------------------}
    
>   ppNotInScope :: String -> ShowS
>   ppNotInScope n =
>       ppTab .
>       showString "Not in scope: " .
>       ppId (showString n)

>   ppPartialTyFun :: String -> ShowS
>   ppPartialTyFun n =
>       ppTab .
>       ppId (showString n) .
>       showString " is not applied to enough arguments."

>   ppInstanceTyFun :: SType -> ShowS
>   ppInstanceTyFun st = 
>       ppTab .
>       showString "Type function in instance head: " .
>       ppType st

>   ppMultipleTypings :: String -> PolyType -> PolyType -> ShowS
>   ppMultipleTypings n pt pt' =
>       ppTab .
>       showString "Duplicate type signatures for " . 
>       ppId (showString n) .
>       showString ":\n" .
>       ppIndent 2 .
>       showString n .
>       ppTyping .
>       ppPolyType pt .
>       ppNewLine .
>       ppIndent 2 .
>       showString n .
>       ppTyping .
>       ppPolyType pt' 

>   ppOverlappingInst :: String -> Instance -> Instance -> ShowS
>   ppOverlappingInst n is is' =
>       showString "\tFound an instance for " .
>       ppId (showString n) .
>       showString ":\n\t\t" . 
>       ppInstance n is .
>       showString "\n\twhich overlaps another instance:\n\t\t" .
>       ppInstance n is'
    
>   ppErrAlias :: Loc AliasDefinition -> ShowS
>   ppErrAlias (Loc d p) =
>       ppId (showString (aDefName d)) .
>       ppSpace .
>       ppPosn p
    
>   ppRefAlias :: Loc AliasDefinition -> ShowS
>   ppRefAlias al =
>       ppTab .
>       showString "references " .
>       ppErrAlias al .
>       showString " which"
    
>   ppCyclicAlias :: [Loc AliasDefinition] -> ShowS
>   ppCyclicAlias (al : als) =
>       ppTab .  
>       showString "The definitions of the following type aliases form a cycle:\n" .
>       ppIndent 3 .
>       showString "   " .
>       ppErrAlias al .
>       ppNewLine .
>       ppDefsW ppRefAlias ppNewLine als .
>       ppNewLine .
>       ppTab .
>       showString "references " .
>       ppErrAlias al

>   ppSuperClass :: String -> ShowS
>   ppSuperClass n =
>       ppTab .
>       showString "is a superclass of " .
>       ppId (showString n) .
>       showString " which"

>   ppCyclicClass :: [String] -> ShowS
>   ppCyclicClass (cl : cls) =
>       ppTab .
>       showString "The hierarchy of the following type classes forms a cycle:\n" .
>       ppIndent 3 .
>       showString "type class " . 
>       ppId (showString cl) . 
>       ppNewLine .
>       ppDefsW ppSuperClass ppNewLine cls .
>       ppNewLine .
>       ppTab .
>       showString "is a superclass of " . 
>       ppId (showString cl) 
    
    {----------------------------------------------------------------------}
    {-- Utility Functions                                                 -}
    {----------------------------------------------------------------------}
   
    Looks up a type in the set of assumptions returned by the type 
    inference which gives us a value of type Maybe PolyType. If this value 
    is of form Just pt then we promote it to a kind, otherwise we raise
    an error.
   
>   lookupKind :: String -> Conversion Kind
>   lookupKind n = gets cdAssumps >>= \as -> case M.lookup ('t':n) as of
>       (Just pt) -> return (promotePolyType pt)
>       Nothing   -> failConv UnknownPos $ ppNotInScope n

>   isAlias :: AlEnv -> String -> Maybe Alias
>   isAlias als n = M.lookup n als

    {----------------------------------------------------------------------}
    {-- STypes -> Types                                                   -}
    {----------------------------------------------------------------------}  
    
>   mkTyFun :: String -> Kind -> Int -> [MonoType] -> Conversion MonoType
>   mkTyFun n k a ps
>       | a > length ps = failConv UnknownPos $ ppPartialTyFun n
>       | otherwise     = return $ foldl TApp (TFun (TyFun n k) xs) ys
>                           where
>                               (xs,ys) = splitAt a ps
    
>   fromType :: Envs -> [MonoType] -> SType -> Conversion MonoType
>   fromType env ps (STyVar n) = return $ 
>       foldl TApp (TVar $ TyVar n KStar) ps
>   fromType env ps (STyCtr n) = do
>       k    <- lookupKind n
>       env' <- gets cdEnvs
>       case isAlias (alEnv env `M.union` alEnv env') n of
>           (Just a) -> mkTyFun n k (aliasArity a) ps
>           Nothing  -> return $ foldl TApp (TCtr $ TyCtr n k) ps
>   fromType env ps (STyApp f a) = do
>       at <- fromType env [] a
>       ft <- fromType env (at:ps) f
>       return ft
>   fromType env [] (STyTuple as) = do
>       tt <- fromType env [] (STyCtr "(,)")
>       ts <- mapM (fromType env []) as
>       return $ foldl TApp tt ts
>   fromType env [] (STyList t) = do
>       lt <- fromType env [] (STyCtr "[]")
>       at <- fromType env [] t
>       return $ TApp lt at
    
>   fromContext :: [TypeConstraint] -> Context
>   fromContext = S.fromList . map (\(TyConstr n v) -> In n $ TVar $ TyVar v KStar) 
    
>   fromTyQual :: Envs -> TyQual SType -> Conversion (Qual MonoType)
>   fromTyQual env (ctx :==> t) = do
>       mt <- fromType env [] t
>       return $ fromContext ctx :=> mt
    
>   fromTyScheme :: Envs -> TyScheme -> Conversion PolyType
>   fromTyScheme env (Scheme ns st) = do
>       qt <- fromTyQual env st
>       return $ ForAll [] qt

>   mkGensFrom :: Int -> [String] -> Theta
>   mkGensFrom m ns = M.fromList $ zip vs [TGen n | n <- [m..]]
>       where
>           vs = map (flip TyVar KStar) ns

>   mkGens :: [String] -> Theta
>   mkGens = mkGensFrom 0

>   paramsToGens :: [TypeParam] -> Theta
>   paramsToGens = mkGens . map tyParamName 
   
    {----------------------------------------------------------------------}
    {-- ADTs                                                              -}
    {----------------------------------------------------------------------}
    
>   mkCtrType :: [TypeParam] -> [Kind] -> TyScheme -> EnvsTrans PolyType
>   mkCtrType ps ks st = do
>       env <- get
>       mt  <- mkMono <$> lift (fromTyScheme env st)
>       return $ ForAll ks (S.empty :=> (paramsToGens ps ~> mt))

>   mkCtr :: [TypeParam] -> [Kind] -> Typed DataConstructor -> EnvsTrans (String, PolyType)
>   mkCtr ps ks (Ty ctr st) = do
>       t <- mkCtrType ps ks st
>       return (dCtrName ctr, t)
    
    Converts a surface data declaration to an ADT.
    
>   mkADT :: DataDefinition -> EnvsTrans ADT
>   mkADT (DDef _ n ps ctrs _) = do
>       k  <- lift $ lookupKind n
>       as <- M.fromList <$> mapM (mkCtr ps (kindArgs k)) (map unL ctrs)
>       return $ ADT k as

    {----------------------------------------------------------------------}
    {-- Aliases                                                           -}
    {----------------------------------------------------------------------}

>   mkAliasType :: [TypeParam] -> [Kind] -> SType -> EnvsTrans PolyType
>   mkAliasType ps ks st = do
>       env <- get
>       mt  <- lift (fromType env [] st)
>       return $ ForAll ks (S.empty :=> paramsToGens ps ~> mt)
    
>   mkAlias :: AliasDefinition -> EnvsTrans Alias
>   mkAlias (ADef n ps st) = do
>       k <- lift $ lookupKind n
>       t <- mkAliasType ps (kindArgs k) st
>       return $ Alias k (length ps) t
    
    {----------------------------------------------------------------------}
    {-- Type Classes                                                      -}
    {----------------------------------------------------------------------}
    
>   mkClContext :: String -> [String] -> [Kind] -> Context
>   mkClContext n ps ks = S.fromList $ map (\(v,k) -> In n $ TVar $ TyVar v k) (zip ps ks)
    
    In order to construct assumptions for a type class, we cannot simply
    convert their surface types to types. We also need to infer their kinds.
    Each declared type may introduce it's own type variables, so we can't
    simply take the kinds from the type class.
    
>   mkClAssump :: Assumps -> Theta -> Context -> [Kind] -> DecType -> Assumps -> EnvsTrans Assumps
>   mkClAssump as s ctx ks (DecTy ns st) rs = do
>       env <- get
>       ct  <- lift (fromTyScheme env st)
>       as' <- lift (gets cdAssumps)
>       case inferPolyKind (as `M.union` as') st of
>            (Left err) -> failTrans UnknownPos $ ppKindError err 
>            (Right k)  -> let
>               ks' = ks ++ kindArgs k
>               mt' = s ~> mkMono ct
>               ptx = context ct
>               s'  = mkGensFrom (length ks) (map (\(TyVar v k) -> v) $ S.toList $ tyVars mt')
>               pt  = ForAll ks' (ctx `S.union` (s' ~> ptx) :=> s' ~> mt') in return $ rs `M.union` (M.fromList $ zip ns $ repeat pt)
    
>   mkClAssumps :: [String] -> [Kind] -> Assumps
>   mkClAssumps ns = M.fromList . zip ns . map (mkPoly . demoteKind)
    
>   mkTyClass :: STypeClass Loc -> EnvsTrans TypeClass
>   mkTyClass (TyClass ctx n ps ds) = do
>       k  <- lift $ lookupKind n
>       let
>           ks = kindArgs k
>           as = mkClAssumps ps ks
>           s  = mkGens ps
>           cs = s ~> mkClContext n ps ks
>       rs <- foldrM (mkClAssump as s cs ks) M.empty (map unL ds)
>       return $ TypeClass (s ~> fromContext ctx) k rs
    
    {----------------------------------------------------------------------}
    {-- Instances                                                         -}
    {----------------------------------------------------------------------}
   
>   isInstEq :: InstDef Loc -> Bool
>   isInstEq (InstValDef _) = True
>   isInstEq _              = False
   
>   mkDictionary :: SInstance Loc -> [Equation]
>   mkDictionary = map instValDef . filter isInstEq . instBody
   
    Checks that the type at the instance head is not a type alias.
   
>   checkInstType :: Pos -> SInstance Loc -> EnvsTrans ()
>   checkInstType p inst = do
>       env  <- get
>       env' <- lift (gets cdEnvs)
>       case isAlias (alEnv env `M.union` alEnv env') (instHeadType inst) of
>           Nothing  -> return ()
>           (Just _) -> failTrans p $ ppInstanceTyFun (instParam inst)
   
>   mkInstance :: Pos -> SInstance Loc -> EnvsTrans Instance
>   mkInstance p inst = do
>       checkInstType p inst
>       env <- get
>       as  <- lift $ (gets cdAssumps)
>       qt  <- lift $ fromType env [] (instParam inst)
>       case inferInstance as inst of
>           (Left err) -> failTrans p $ ppKindError err 
>           (Right k)  -> do
>               let
>                   cst = instConstrs inst
>                   st  = instParam inst
>                   vs  = S.toList $ freeTyVarsN (cst :==> st)
>                   ts  = zipWith (\n -> TVar . TyVar n) vs (kindArgs k)
>                   s   = M.fromList $ zip (flip TyVar KStar <$> vs) ts 
>                   ctx = fromContext cst
>               return $ Inst (s ~> ctx :=> s ~> qt) (mkDictionary inst)
   
    {----------------------------------------------------------------------}
    {-- Explicit typings                                                  -}
    {----------------------------------------------------------------------}
    
>   mkExplicit :: DecType -> EnvsTrans PolyType
>   mkExplicit (DecTy _ st) = do
>       env <- get
>       t   <- lift $ fromTyScheme env st
>       as  <- lift (gets cdAssumps)
>       case inferPolyKind as st of
>            (Left err) -> error $ ppKindError err ""
>            (Right k)  -> let
>               mt  = mkMono t
>               ctx = context t
>               ks  = kindArgs k
>               s   = mkGens (map (\(TyVar v k) -> v) $ S.toList $ tyVars mt)
>               pt  = ForAll ks (s ~> ctx :=> s ~> mt) in return pt 
    
>   mkExpl :: Pos -> PolyType -> String -> EnvsTrans ()
>   mkExpl p pt n = do
>       e <- get
>       case addExpl n pt e of 
>           (Right e') -> put e'
>           (Left pt') -> failTrans p $ ppMultipleTypings n pt pt'
    
    {----------------------------------------------------------------------}
    {-- State Types                                                       -}
    {----------------------------------------------------------------------}
   
>   mkState :: StateDefinition -> EnvsTrans ()
>   mkState (SDef _ _ p _ al adt _) = do
>       al'  <- mkAlias al
>       adt' <- mkADT adt
>       modify $ addADT (dDefName adt) adt'
>       modify $ addAlias (aDefName al) al'
>       modify $ addSt (dDefName adt) (case p of
>           Nothing  -> Simple
>           (Just _) -> Transformer)
    
    {----------------------------------------------------------------------}
    {-- Environments                                                      -}
    {----------------------------------------------------------------------}
    
>   mkDec :: LocP Definition -> EnvsTrans ()
>   mkDec (Loc (DataDef d) p) = do
>       adt <- mkADT d
>       modify $ addADT (dDefName d) adt
>   mkDec (Loc (TyClDef d) p) = do
>       cl <- mkTyClass d
>       modify $ addClass (tyClName d) cl
>   mkDec (Loc (TypeDec d) p) = do
>       ex <- mkExplicit d
>       mapM_ (mkExpl p ex) (defSigNames d)
>   mkDec (Loc (InstDef d) p) = do
>       is  <- mkInstance p d
>       env <- get
>       inv <- lift (gets cdEnvs)
>       case mergeEnvs env inv of
>           (Left err)  -> failTrans UnknownPos (showString "merge error in mkDec (Conversion)")
>           (Right cnv) ->  case findOverlap (instClass d) cnv is of
>               (Just is') -> failTrans p $ 
>                   ppOverlappingInst (instClass d) is is'
>               Nothing    -> put $ 
>                   addInstance (instClass d) is env
>   mkDec (Loc (StateDef d) p) = do mkState d
>   mkDec _            = do return ()

>   mkAliases :: Loc AliasDefinition -> EnvsTrans ()
>   mkAliases (Loc d p) = do
>       al <- mkAlias d
>       modify $ addAlias (aDefName d) al
    
>   checkAlias :: SCC AliasNode -> Conversion (Loc AliasDefinition)
>   checkAlias (AcyclicSCC n)     = return (anDef n)
>   checkAlias (CyclicSCC (n:ns)) = failConv (anLoc n) $ 
>       ppCyclicAlias $ map anDef (n:ns)

>   checkClass :: SCC ClassNode -> Conversion ()
>   checkClass (CyclicSCC (n:ns)) = failConv UnknownPos $
>       ppCyclicClass $ map clName (n:ns)
>   checkClass _ = return ()

>   mkEnvs' :: Module Loc -> Conversion Envs
>   mkEnvs' m = do
>       als <- mapM checkAlias (sortAliases ds)
>       env <- execStateT (mapM_ mkAliases als >> mapM_ mkDec ds) initialEnvs
>       mapM_ checkClass $ sortClasses (clEnv env)
>       return env
>       where
>           ds = moduleDefs m
    
>   mkEnvs :: Envs -> Assumps -> Module Loc -> Either [Loc String] Envs
>   mkEnvs env as m = toEither $ evalStateT (mkEnvs' m) (CD as env)

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
