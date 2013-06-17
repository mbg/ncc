{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}



> module TypeSystem.Sort (
>   runGrouping
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}

>   import Utility.PrettyPrint
    
>   import Control.Applicative ((<$>))
>   import Control.Monad.State
    
>   import Data.Graph
>   import qualified Data.Map as M
>   import qualified Data.Set as S
>   import Data.Maybe (fromJust)

>   import Cada.AST
>   import Cada.STypes

>   import TypeSystem.Kind
>   import TypeSystem.Types
>   import TypeSystem.Substitution
>   import TypeSystem.PolyType
>   import TypeSystem.BindGroup
>   import TypeSystem.Assump
>   import TypeSystem.Instance
>   import TypeSystem.Environments
>   import TypeSystem.Conversion

>   import Compiler.Monad

>   import Utility.Map

    {----------------------------------------------------------------------}
    {-- Value Groups                                                      -}
    {----------------------------------------------------------------------}

    Type signatures and equations are all parsed seperately. A value groups
    combines them all for a given binding.
    
>   data ValGroup = ValGroup {
>       valType :: Maybe PolyType,
>       valEqs  :: [Equation]
>   }       

>   type ValGroups  = M.Map String ValGroup
>   type InstGroups = M.Map Constr ValGroups 

    {----------------------------------------------------------------------}
    {-- Grouping Monad                                                    -}
    {----------------------------------------------------------------------}

>   data GroupingState = GS {
>       groups     :: ValGroups,
>       instGroups :: InstGroups,
>       envs       :: Envs
>   }
    
>   type Group = StateT GroupingState Compiler

>   withValGroups :: (ValGroups -> Compiler ValGroups) -> Group ()
>   withValGroups f = do
>       s  <- get
>       gs <- lift $ f (groups s)
>       put $ s { groups = gs }   

>   withInstGroups :: (InstGroups -> Compiler InstGroups) -> Group ()
>   withInstGroups f = do
>       s  <- get
>       gs <- lift $ f (instGroups s)
>       put $ s { instGroups = gs }

>   runGrouping :: Envs -> Module Loc -> Compiler BindGroup 
>   runGrouping e m = evalStateT (sortValueDefs m) $ GS M.empty M.empty e

>   sortValueDefs :: Module Loc -> Group BindGroup
>   sortValueDefs m = do
>       groupDefs m
>       groupInstDecls 
>       bg <- toBG
>       return bg

    {----------------------------------------------------------------------}
    {-- Grouping                                                          -}
    {----------------------------------------------------------------------}
    
>   addType :: PolyType -> String -> ValGroups -> Compiler ValGroups
>   addType t n gs = case M.lookup n gs of
>       Nothing   -> return $ M.insert n (ValGroup (Just t) []) gs
>       (Just vn) -> case valType vn of
>           Nothing  -> return $ M.insert n (ValGroup (Just t) (valEqs vn)) gs
>           (Just _) -> error "duplicate type signature for n"

>   addValType :: PolyType -> String -> Group ()
>   addValType t n = withValGroups (addType t n)

>   addInstType :: PolyType -> String -> Constr -> Group ()
>   addInstType t n i = withInstGroups $ \gs -> case M.lookup i gs of
>       (Just vgs) -> do
>           vgs' <- addType t n vgs
>           return $ M.insert i vgs' gs
>       Nothing    -> do
>           vgs <- addType t n M.empty
>           return $ M.insert i vgs gs

>   addEquation :: Equation -> ValGroups -> Compiler ValGroups
>   addEquation eq gs = case M.lookup (eqName eq) gs of
>       Nothing   -> return $ M.insert (eqName eq) (ValGroup Nothing [eq]) gs
>       (Just vn) -> return $ M.insert (eqName eq) (ValGroup (valType vn) (eq : valEqs vn)) gs

>   addValEq :: Equation -> Group ()
>   addValEq eq = withValGroups $ addEquation eq 

>   addInstEq :: Constr -> Equation -> Group ()
>   addInstEq i eq = withInstGroups $ \gs -> case M.lookup i gs of
>       (Just vgs) -> do
>           vgs' <- addEquation eq vgs
>           return $ M.insert i vgs' gs
>       Nothing    -> do
>           vgs <- addEquation eq M.empty
>           return $ M.insert i vgs gs

>   getInstEqs :: Constr -> Group ValGroups
>   getInstEqs i = do
>       gs <- gets instGroups
>       case M.lookup i gs of
>           (Just vgs) -> return vgs
>           Nothing    -> error $ "getInstEqs: no instance methods for " ++ ppConstr i ""

>   lookupClsMethodTy :: String -> String -> Group PolyType
>   lookupClsMethodTy cls n = do
>       e <- gets envs
>       case getClsExpl cls n e of
>           Nothing  -> fail $ "Not in scope: class method " ++ n
>           (Just t) -> return t

>   lookupExpl :: String -> Group PolyType
>   lookupExpl n = do
>       e <- gets envs
>       case getExpl n e of
>           Nothing  -> fail $ "Not in scope: " ++ n
>           (Just t) -> return t

>   addTypeDecs :: DecType -> Group ()
>   addTypeDecs (DecTy ns ts) = do
>       t <- lookupExpl (head ns) 
>       mapM_ (addValType t) ns

>   isClsConstr :: Constr -> Bool
>   isClsConstr (In _ (TGen 0)) = False
>   isClsConstr _               = True

>   adjustConstr :: Constr -> Constr
>   adjustConstr (In n (TGen i)) = In n (TGen (i-1))

>   specCtx :: Context -> Context
>   specCtx = S.map adjustConstr . S.filter isClsConstr

>   specMethodTy :: Context -> Constr -> String -> ValGroup -> Group ()
>   specMethodTy ptx i@(In cls mt) n _ = do
>       pt@(ForAll (k:ks) (ctx :=> mt')) <- lookupClsMethodTy cls n
>       let
>           vs   = S.toList (tyVars mt)
>           ks'  = map kind vs
>           ts   = map (TGen . fst) $ zip [0..] (ks ++ ks')
>           ctx' = specCtx ctx
>           s    = M.fromList $ zip vs $ drop (length ks) ts
>           mt'' = s ~> mt
>           pt'  = ForAll (ks ++ ks') (ctx' `S.union` (s ~> ptx) :=> inst (mt'' : ts) mt')
>       addInstType pt' n i

>   addInstEqs :: String -> Instance -> Group () 
>   addInstEqs _ (Inst (ctx :=> mt) [])  = do return ()
>   addInstEqs n (Inst (ctx :=> mt) eqs) = do
>       let cs = In n mt
>       mapM_ (addInstEq cs) eqs
>       vgs <- getInstEqs cs
>       mapFoldrM_ (specMethodTy ctx cs) vgs

>   addData :: DataDefinition -> Group ()
>   addData = mapM_ addValEq . dDefProjs

>   addState :: StateDefinition -> Group ()
>   addState (SDef _ _ _ _ _ adt eqs) = do
>       addData adt
>       mapM_ addValEq eqs

>   valDef :: Definition Loc -> Group ()
>   --valDef (TyClDef d)  = addTypeClass d
>   --valDef (InstDef d)  = addInstEqs d
>   valDef (DataDef d)  = addData d
>   valDef (StateDef d) = addState d
>   valDef (TypeDec d)  = addTypeDecs d
>   valDef (ValueDef d) = addValEq d
>   valDef _            = return ()
    
>   groupDefs :: Module Loc -> Group () 
>   groupDefs = mapM_ valDef . map unL . moduleDefs

>   groupInstDecls :: Group ()
>   groupInstDecls = do
>       env <- gets envs 
>       mapFoldrM_ (\k is -> mapM_ (addInstEqs k) is) (inEnv env) 

    {----------------------------------------------------------------------}
    {-- Assumptions                                                       -}
    {----------------------------------------------------------------------}
    
    {----------------------------------------------------------------------}
    {-- Sorting                                                           -}
    {----------------------------------------------------------------------}
    
>   isExpl :: ValGroup -> Bool
>   isExpl (ValGroup Nothing _) = False
>   isExpl _                    = True

>   isImpl :: ValGroup -> Bool
>   isImpl = not . isExpl

>   toExpl :: String -> ValGroup -> Expl
>   toExpl n (ValGroup pt alts) = Expl n (fromJust pt) (reverse alts)

>   toImpl :: SCC ValueNode -> [Impl]
>   toImpl (AcyclicSCC (eqs,n,_)) = [Impl n eqs]
>   toImpl (CyclicSCC ks)         = map (\(eqs,n,_) -> Impl n (reverse eqs)) ks
    
>   instName :: MonoType -> String
>   instName (TCtr (TyCtr n _)) = n
>   instName (TApp l _) = instName l
    
>   toInsts :: (Constr, ValGroups) -> Insts
>   toInsts ((In n mt), vgs) = Insts n (S.empty :=> mt) $ 
>       map (\(n,vg) -> toExpl (n ++ instName mt) vg) (M.toList vgs)
    
>   type ValueNode = ([Equation], String, [String])

>   toValueNode :: String -> ValGroup -> ValueNode
>   toValueNode n vn = (valEqs vn, n, S.toList $ freeVars $ valEqs vn)

>   findDeps :: ValGroups -> [SCC ValueNode]
>   findDeps = stronglyConnCompR . M.elems . M.mapWithKey toValueNode

>   toBG :: Group BindGroup
>   toBG = do
>       vs <- gets groups
>       is <- gets instGroups
>       return $ BG (mkExpl vs) (mkImpls vs) (mkInsts is)
>       where
>           mkExpl  = M.elems . M.mapWithKey toExpl . M.filter isExpl
>           mkImpls = map toImpl . findDeps . M.filter isImpl
>           mkInsts = map toInsts . M.toList

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
