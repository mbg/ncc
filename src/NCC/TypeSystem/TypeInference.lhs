{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module TypeSystem.TypeInference (
>   runTI,
>   tiBindGroup,
>   inferTypes
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Debug.Trace (trace)
>   import Utility.PrettyPrint
    
>   import Control.Applicative ((<$>))
>   import Control.Monad
>   import Control.Monad.Error
    
>   import qualified Data.List as L
>   import Data.List.Split (splitOn)
>   import qualified Data.Map as M
>   import qualified Data.Set as S
    
>   import Cada.AST hiding (Typed(..))
>   import Cada.PrettyPrint 
    
>   import TypeSystem.Types
>   import TypeSystem.Substitution hiding ((<>))
>   import TypeSystem.PolyType
>   import TypeSystem.TypeClass
>   import TypeSystem.Instance
>   import TypeSystem.Unify
>   import TypeSystem.Monad 
>   import TypeSystem.PreludeTypes
>   import TypeSystem.Assump
>   import TypeSystem.BindGroup
>   import TypeSystem.TypeError
>   import TypeSystem.Environments
>   import TypeSystem.Reduction
>   import TypeSystem.Tags
>   import TypeSystem.Typed

>   import Internal.NCC

>   import Utility.Set

    {----------------------------------------------------------------------}
    {-- Type Inference                                                    -}
    {----------------------------------------------------------------------}

>   type Infer e t = Envs -> Assumps -> e -> TI (Context, t)

>   data InferResult a = IR {
>       resultContext :: Context,
>       resultAssumps :: Assumps,
>       resultValue   :: a
>   }

>   type InferR e t = Envs -> Assumps -> e -> TI (InferResult (Typed e t))

>   returnVal :: e -> t -> TI (InferResult (Typed e t))
>   returnVal r t = return $ IR S.empty M.empty $ Ty r t

>   returnWithCtx :: Context -> e -> t -> TI (InferResult (Typed e t))
>   returnWithCtx ctx r t = return $ IR ctx M.empty $ Ty r t

>   returnWithAssumps :: Assumps -> e -> t -> TI (InferResult (Typed e t))
>   returnWithAssumps as r t = return $ IR S.empty as $ Ty r t

>   returnInfer :: Context -> Assumps -> e -> t -> TI (InferResult (Typed e t))
>   returnInfer ctx as r t = return $ IR ctx as $ Ty r t

    {----------------------------------------------------------------------}
    {-- Literals                                                          -}
    {----------------------------------------------------------------------}

    Type inference for simple literals is easy: we just return the appropriate
    type constructor (defined in Internal.NCC).

>   tiLiteral :: InferR Literal MonoType 
>   tiLiteral env as r@UnitLit      = returnVal r unitType
>   tiLiteral env as r@(StrLit _)   = returnVal r stringType
>   tiLiteral env as r@(IntLit _)   = returnVal r intType

    We only support pairs (2-tuples) at the moment. This function would
    have to be modified to select an appropriate type constructor for
    larger arities.

>   tiLiteral env as r@(PairLit es) = do
>       (ctx, t) <- tiExpr env as $ foldl App (Var "(,)") es
>       returnWithCtx ctx r t

    For lists, we infer the types of all elements and try to unify them
    with the generic list constructor.

>   tiLiteral env as r@(ListLit es) = do
>       (ctx,ts)     <- tiExprs env as es
>       (ctx' :=> t) <- newInst listPolyType
>       mapM (unify env t) (map (TApp listType) ts)  
>       returnWithCtx (ctx `S.union` ctx') r t 
    
    {----------------------------------------------------------------------}
    {-- Patterns                                                          -}
    {----------------------------------------------------------------------}
    
>   tiPatExpr :: Assumps -> Expr -> TI (Context, Assumps, MonoType)
>   tiPatExpr as (Var x) = do
>       v <- newTyVar KStar
>       return (S.empty, x ~= mkPoly v, v)

>   tiPatExprs :: Assumps -> [Expr] -> TI (Context, Assumps, [MonoType])
>   tiPatExprs as es = do
>       rs <- mapM (tiPatExpr as) es
>       let
>           ctx = S.unions [c | (c,_,_) <- rs]
>           as' = unions [a | (_,a,_) <- rs]
>           ts  = [t | (_,_,t) <- rs]
>       return (ctx, as', ts) 
    
>   type LocalType    = MonoType
>   type LocalAssumps = Assumps

>   tiPatLiteral :: InferR Literal MonoType 
>   tiPatLiteral env as r@UnitLit     = returnVal r unitType 
>   tiPatLiteral env as r@(StrLit _)  = returnVal r stringType 
>   tiPatLiteral env as r@(IntLit _)  = do
>       v <- newTyVar KStar
>       returnWithCtx (S.singleton (In "Num" v)) r v
>   tiPatLiteral env as r@(PairLit es) = do
>       (ctx,as',ts) <- tiPatExprs as es
>       t'           <- newTyVar KStar
>       pt           <- find "(,)" as
>       (ctx' :=> t) <- newInst pt 
>       unify env t (foldr mkFun t' ts) 
>       s <- getTheta
>       returnInfer (ctx `S.union` ctx') as' r t' 
>   tiPatLiteral env as r@(ListLit es) = do
>       (ctx,as',ts) <- tiPatExprs as es
>       (ctx' :=> t) <- newInst listPolyType
>       s <- getTheta
>       mapM (unify env t) (map mkListType ts)  
>       returnInfer (ctx `S.union` ctx') as' r t 

>   tiPattern :: InferR Pattern MonoType --Envs -> Assumps -> Pattern -> TI (Context, Assumps, MonoType, LocalType)
>   tiPattern _ _ r@Wildcard          = do
>       v <- newTyVar KStar
>       returnVal r v
>   tiPattern _ _ r@(VarPattern x)    = do 
>       v  <- newTyVar KStar
>       v' <- newTyVar KStar 
>       returnWithAssumps (x ~= mkPoly v) r v
>   tiPattern env as r@(LitPattern l)   = tiPatLiteral env as l >>= \(IR c as (Ty v t)) -> return $ IR c as (Ty r t)
>   tiPattern env as r@(CtrPattern f ps) = do
>       (ctx, as', ts) <- tiPatterns env as ps
>       t'             <- newTyVar KStar
>       pt             <- find f as
>       (ctx' :=> t)   <- newInst pt 

>       case getTags f env of
>           Nothing   -> unify env t (foldr mkFun t' ts)
>           (Just rs) -> case findRule rs ps of
>               Nothing  -> unify env t (foldr mkFun t' ts)
>               (Just r) -> do 
>                   (ctx'' :=> t'') <- newInst (tagRuleType r)
>                   unify env t'' (foldr mkFun t' (drop (length (tagRulePattern r)) ts))
>       returnInfer (ctx `S.union` ctx') as' r t'

>       --unify env t (foldr mkFun t' ts) 

    We need to test if the constructor has any associated tags.

>       {-t0             <- newTyVar KStar
>       case getTags f env of
>           Nothing   -> do
>               mapM_ (\(x, y) -> unify env x y) (zip ts ls)
>               unify env t' t0
>           (Just rs) -> case findRule rs ps of

                If there is no matching rule, we can't refine the type.

>               Nothing  -> do
>                   mapM_ (\(x, y) -> unify env x y) (zip ts ls)         
>                   unify env t' t0      

                Otherwise, we need to refine the types of the patterns.

>               (Just r) -> do
>                   (ctx'' :=> t'') <- newInst (tagRuleType r)
>                   unify env t'' (foldr mkFun t0 (drop (length (tagRulePattern r)) ls))

>       returnInfer (ctx `S.union` ctx') as' r (t', t0)-}

   tiPatternExt :: Envs -> Assumps -> Pattern -> TI (Context, Assumps, MonoType)
   tiPatternExt env as p = do
       (ctx, as', t, lt) <- tiPattern env as p 
       unify env t lt
       return (ctx,as',t)

>   tiPatterns :: Envs -> Assumps -> [Pattern] -> TI (Context, Assumps, [MonoType])
>   tiPatterns env as ps = do
>       rs <- mapM (tiPattern env as) ps
>       let
>           ctx = S.unions $ map resultContext rs 
>           as' = unions $ map resultAssumps rs 
>           ts  = map (tyType . resultValue) rs 
>           --ls  = map (snd . tyType . resultValue) rs
>       return (ctx, as', ts) 

    {----------------------------------------------------------------------}
    {-- Expressions                                                       -}
    {----------------------------------------------------------------------}

>   tiOption :: Envs -> Assumps -> MonoType -> MonoType -> Option -> TI Context
>   tiOption env as t rt opt = tiOption' env as t rt opt `inContext` 
>       \err -> OptionError opt err

>   tiOption' :: Envs -> Assumps -> MonoType -> MonoType -> Option -> TI Context
>   tiOption' env as t rt (Option p e) = do
>       (IR ctx as' (Ty _ t')) <- tiPattern env as p -- TODO: do something with l
>       unify env t t'
>       (dtx, t'') <- tiExpr env (as' `M.union` as) e
>       unify env rt t''
>       return (ctx `S.union` dtx)
    
>   tiExpr :: Infer Expr MonoType
>   tiExpr env as e = tiExpr' env as e `inContext` \err -> ExprError e err
    
>   tiExpr' :: Infer Expr MonoType
>   tiExpr' env as (Var x) = do
>       pt          <- find x as
>       (ctx :=> t) <- newInst pt
>       return (ctx, t)
>   tiExpr' env as (Ctr x) = do
>       pt          <- find x as
>       (ctx :=> t) <- newInst pt

        A constructor may have associated tag rules. 

>       case getTags x env of
>           Nothing   -> return ()
>           (Just rs) -> return ()

>       return (ctx, t)        
>   tiExpr' env as (Lit l) = tiLiteral env as l >>= \r -> return (resultContext r, tyType $ resultValue r)
>   tiExpr' env as (App f a) = do
>       (ctx,  tf) <- tiExpr env as f
>       (ctx', ta) <- tiExpr env as a
>       t          <- newTyVar KStar
>       unifyAbs env a (ta `mkFun` t) f tf
>       return (ctx `S.union` ctx', t)
>   tiExpr' env as (InfixOp o x y) = do

    An InfixOp `x o y' is just syntactic sugar for `((o x) y)':
    
>       tiExpr' env as (App (App (Var o) x) y)
    
>   tiExpr' env as (Abs alt)    = tiAlt env as alt
>   tiExpr' env as (Do stmts)   = tiStmts env as stmts
>   tiExpr' env as (Cond ce te fe) = do
>       (ctx,t)   <- tiExpr env as ce
>       unify env t boolType
>       (ttx,t')  <- tiExpr env as te
>       (ftx,t'') <- tiExpr env as fe
>       unify env t' t'' 
>       return (ctx `S.union` ttx `S.union` ftx, t')
>   tiExpr' env as (Let alts e) = do
>       (ctx, as') <- tiLetGroup env as alts
>       (dtx, t)   <- tiExpr env (as' `M.union` as) e
>       return (ctx `S.union` dtx, t)
>   tiExpr' env as (Case e os) = do
>       (ctx, t) <- tiExpr env as e
>       t'       <- newTyVar KStar
>       ctxs     <- mapM (tiOption env as t t') os
>       return (ctx `S.union` S.unions ctxs, t')

>   tiExprs :: Infer [Expr] [MonoType]
>   tiExprs env as es = do
>       rs <- mapM (tiExpr env as) es
>       let
>           ctx = S.unions [c | (c,_) <- rs]
>           ts  = [t | (_,t) <- rs]
>       return (ctx, ts) 

    {----------------------------------------------------------------------}
    {-- Statements                                                        -}
    {----------------------------------------------------------------------}   

>   internalGetterName :: String
>   internalGetterName = "__internal_tmp_getter"

>   tiGetter' :: Envs -> Assumps -> [String] -> TI (Context, MonoType)
>   tiGetter' env as [n] = do
>       pt            <- find n as
>       (ntx :=> nmt) <- newInst pt
>       --tt            <- find internalGetterName as
>       --(ttx :=> tmt) <- newInst pt
>       --t             <- newTyVar KStar
>       --unify env (tmt `mkFun` t) nmt
>       --return (ntx `S.union` ttx, t)
>       return (ntx, nmt)
>   tiGetter' env as (n:ns) = do
>       pt            <- find n as
>       (ntx :=> nmt) <- newInst pt
>       --tt            <- find internalGetterName as
>       --(ttx :=> tmt) <- newInst pt
>       --t             <- newTyVar KStar
>       --unify env (tmt `mkFun` t) nmt
>       --return (ntx `S.union` ttx, t)
>       (mtx, mmt)    <- tiGetter' env as ns
>       tx            <- newTyVar KStar
>       ta            <- newTyVar KStar
>       tb            <- newTyVar KStar
>       unify env (tx `mkFun` ta) nmt
>       unify env (ta `mkFun` tb) mmt
>       return (ntx `S.union` mtx, tx `mkFun` tb)

>   tiGetter :: Envs -> Assumps -> [String] -> TI (Context, MonoType)
>   tiGetter env as [n] = do
>       pt           <- find (n ++ ".get") as
>       (ntx :=> nt) <- newInst pt
>       return (ntx,nt)
>   tiGetter env as (n:ns) = do
>       pt           <- find (n ++ ".get") as
>       (ntx :=> nt) <- newInst pt
>       pt'          <- find ">>=" as
>       (btx :=> bt) <- newInst pt'
>       v            <- newTyVar KStar
>       (nsctx,nst)  <- tiGetter' env ((internalGetterName ~= mkPoly v) <> as) ns
>       r            <- newTyVar KStar
>       unify env (v `mkFun` r) nst
>       rt           <- find "return" as
>       (rtx :=> rmt) <- newInst rt
>       rm            <- newTyVar KStar
>       unify env (r `mkFun` rm) rmt
>       return (S.empty {-`S.union` btx `S.union` nsctx `S.union` rtx-},rm)

>   tiSetter' :: Envs -> Assumps -> [String] -> Expr -> MonoType -> TI (Context, MonoType)
>   tiSetter' env as [n] e st = do
>       pt           <- find n as           -- contents :: PocketData -> String
>       (ntx :=> nt) <- newInst pt
>       (etx, et)    <- tiExpr' env as e    -- :: String
>       -- we need to check that nt = st -> et
>       unify env (st `mkFun` et) nt
>       return (etx, st)
>   tiSetter' env as (n:ns) e st = do
>       pt           <- find n as           -- leftPocket :: DriverData -> PocketData
>       (ntx :=> nt) <- newInst pt
>       st'          <- newTyVar KStar
>       (etx, et)    <- tiSetter' env as ns e st'    -- :: PocketData
>       -- we need to check that nt = st -> et
>       unify env (st `mkFun` et) nt
>       --unify env st st'
>       return (etx, st)

>   tiSetter :: Envs -> Assumps -> [String] -> Expr -> TI (Context, MonoType)
>   tiSetter env as [n]    e = do tiExpr' env as (App (Var (n ++ ".set")) e)
>   tiSetter env as (n:ns) e = do
>       pt           <- find (n ++ ".modify") as
>       (ntx :=> nt) <- newInst pt
>       st           <- newTyVar KStar              -- st is the initial state type (which we don't know)
>       (rtx, rt)    <- tiSetter' env as ns e st    -- rt is what the setter returns (should be the same as st)
>       ft           <- newTyVar KStar
>       unify env (st `mkFun` rt) ft                -- ft is the argument type of nt
>       rm           <- newTyVar KStar
>       unify env (ft `mkFun` rm) nt                -- nt is the type of n.modify
>       return (ntx `S.union` rtx, rm)

    p <- e      ~>      e   >>= \p -> ...  
    e           ~>      e   >>= \_ -> ...
    p <: n      ~>  n.get   >>= \p -> ...
    e >: n      ~>  n.set e >> ...
    
>   tiStmt :: Envs -> Assumps -> Statement -> TI (Context, MonoType, Pattern)
>   tiStmt env as stmt = tiStmt' env as stmt `inContext` \err -> StmtError stmt err

>   tiStmt' :: Envs -> Assumps -> Statement -> TI (Context, MonoType, Pattern)
>   tiStmt' env as (Statement e) = do
>       pt           <- find ">>=" as
>       (btx :=> bt) <- newInst pt
>       (etx, et)    <- tiExpr env as e
>       t            <- newTyVar KStar
>       unify env (et `mkFun` t) bt
>       return (btx `S.union` etx, t, Wildcard)
>   tiStmt' env as (Bind p e)    = do
>       pt           <- find ">>=" as
>       (btx :=> bt) <- newInst pt
>       (etx, et)    <- tiExpr env as e
>       t            <- newTyVar KStar
>       unify env (et `mkFun` t) bt
>       return (btx `S.union` etx, t, p)
>   tiStmt' env as (Getter p n)  = do
>       pt           <- find ">>=" as
>       (btx :=> bt) <- newInst pt
>       (ntx, nt)    <- tiGetter env as (splitOn "." n)
>       --pt'          <- find (n ++ ".get") as
>       --(ntx :=> nt) <- newInst pt'
>       t            <- newTyVar KStar
>       unify env (nt `mkFun` t) bt
>       return (btx `S.union` ntx, t, p)
>   tiStmt' env as (Setter e n)  = do
>       pt           <- find ">>=" as
>       (btx :=> bt) <- newInst pt
>       --(etx, et)    <- tiExpr env as (App (Var (n ++ ".set")) e)
>       (etx, et)    <- tiSetter env as (splitOn "." n) e
>       t            <- newTyVar KStar
>       unify env (et `mkFun` t) bt
>       return (btx `S.union` etx, t, Wildcard)      

>   tiStmts :: Infer [Statement] MonoType
>   tiStmts env as [Statement e] = tiExpr' env as e
>   tiStmts env as [Setter e n]  = tiSetter env as (splitOn "." n) e --tiExpr' env as (App (Var (n ++ ".set")) e)
>   tiStmts env as (x:xs)        = do
>       (ctx,et,p)   <- tiStmt env as x
>       (IR ptx as' (Ty _ pt)) <- tiPattern env as p -- TODO: do something with lt
>       (stx,st)     <- tiStmts env (as' <> as) xs
>       t            <- newTyVar KStar
>       unify env ((pt `mkFun` st) `mkFun` t) et
>       return (ctx `S.union` ptx `S.union` stx,t)
    
    {----------------------------------------------------------------------}
    {-- Equations                                                         -}
    {----------------------------------------------------------------------}
   
>   tiAlt :: Infer Alt MonoType
>   tiAlt env as (Alt ps e) = do
>       (ctx, as', ts) <- tiPatterns env as ps -- TODO: do something with ls
>       (ctx', t)      <- tiExpr env (as' <> as) e 
>       return (ctx `S.union` ctx', foldr mkFun t ts)
   
>   tiEquation :: Infer Equation MonoType
>   tiEquation env as e = tiEquation' env as e `inContext` EqError e
   
>   tiEquation' :: Infer Equation MonoType
>   tiEquation' env as (Eq _ alt) = tiAlt env as alt

>   tiEquations :: Envs -> Assumps -> [Equation] -> MonoType -> TI Context
>   tiEquations env as es t = do
>       rs <- mapM (tiEquation env as) es
>       mapM (unify env t) (map snd rs) `inContext` EqError (head es)  -- todo: get the right equation
>       return $ S.unions (map fst rs)

    {----------------------------------------------------------------------}
    {-- Context Reduction                                                 -}
    {----------------------------------------------------------------------}
    
>   split :: Envs -> S.Set TyVar -> S.Set TyVar -> Context -> TI (Context, Context)
>   split env fs gs ctx = do
>       ctx' <- reduce env ctx
>       let
>           (ds, rs) = S.partition (setAll (`S.member` fs) . tyVars) ctx'
>       -- TODO: apply defaults
>       return (ds, rs)

>   type Ambiguity = (TyVar, Context)

>   ambiguities :: Envs -> S.Set TyVar -> Context -> [Ambiguity]
>   ambiguities env vs ctx = []
    
    {----------------------------------------------------------------------}
    {-- Explicitly Typed Bindings                                         -}
    {----------------------------------------------------------------------}

    Comment: we can't use (explName ex), because it will return 'unexpected'
    names for instance methods such as 'last[]' rather than 'last'. 
    
>   tiExpl :: Envs -> Assumps -> Expl -> TI Context
>   tiExpl env as ex = tiExpl' env as ex `inContext` 
>       EqsError (eqName $ unL $ head $ explEqs ex)
    
>   tiExpl' :: Envs -> Assumps -> Expl -> TI Context
>   tiExpl' env as (Expl n pt eqs) = do
>       (ctx :=> t) <- newInst pt
>       etx         <- tiEquations env as (map unL eqs) t
>       s           <- getTheta
>       let
>           ctx' = s ~> ctx
>           t'   = s ~> t
>           fs   = tyVars (s ~> as)
>           gs   = tyVars t' S.\\ fs
>           pt'  = quantifyTy gs (ctx' :=> t') 
>           etx' = s ~> etx
>       es      <- filterM (\cs -> not <$> entail env ctx' cs) (S.toList etx')
>       (ds,rs) <- split env fs gs (S.fromList es)
>       if pt /= pt' then do -- TODO: does this work fine with type functions? 
>           s' <- getTheta
>           throwError $ SignatureTooGeneral pt pt'
>       else if not (S.null rs) then do
>           s' <- getTheta
>           throwError $ ContextTooWeak (s' ~> rs)
>       else
>           return ds
    
    {----------------------------------------------------------------------}
    {-- Implicitly Typed Bindings                                         -}
    {----------------------------------------------------------------------}
   
>   tiImpls' :: Envs -> Assumps -> [[Equation]] -> [MonoType] -> TI Context
>   tiImpls' env as eqs ts = fmap S.unions $ sequence $ zipWith (tiEquations env as) eqs ts
   
>   quant :: Context -> MonoType -> PolyType
>   quant ctx t = quantifyTy (tyVars t) (ctx :=> t) 

    does the monomorphism restriction apply?
    test := is this a simple binding?

>   restricted :: [Impl] -> Bool
>   restricted = any simple 
>       where
>           simple (Impl n eqs) = any (null . altPats . eqAlt . unL) eqs
   
>   tiImpls :: Infer [Impl] Assumps
>   tiImpls env as bs = do
>       ts  <- mapM (\_ -> newTyVar KStar) bs
>       ctx <- tiImpls' env (mkAssumps (map implName bs) (map mkPoly ts) <> as) (map (map unL . implEqs) bs) ts
>       s   <- getTheta
>       let
>           ctx' = s ~> ctx
>           ts'  = s ~> ts
>           fs   = tyVars (s ~> as)
>           vss  = map tyVars ts'
>           gs   = foldr S.union S.empty vss S.\\ fs
>       (ds,rs) <- split env fs (foldr S.intersection S.empty vss) ctx'
>       -- TODO: quantify over gs' = gs \\ ttVars rs -and- gs?
>       return (ds, mkAssumps (map implName bs) (map (quant (s ~> rs)) (s ~> ts)))
>       {-if restricted bs then 
>           return (ds `S.union` rs, mkAssumps (map implName bs) (map (quant S.empty) (s ~> ts)))
>       else
>           return (ds, mkAssumps (map implName bs) (map (quant (s ~> rs)) (s ~> ts))) -}

    {----------------------------------------------------------------------}
    {-- Instance Groups                                                   -}
    {----------------------------------------------------------------------}

>   tiInsts :: Envs -> Assumps -> Insts -> TI [Context]
>   tiInsts env as (Insts _ _ es) = do
>       --reset
>       ctxs <- mapM (tiExpl env as) es
>       return ctxs
    
    {----------------------------------------------------------------------}
    {-- Bind Groups                                                       -}
    {----------------------------------------------------------------------}
    
>   tiLetBindingName :: Pattern -> TI String
>   tiLetBindingName (VarPattern x) = return x
>   tiLetBindingName _              = error "tiLetBindingName"
    
>   tiLetAssumps :: Envs -> Assumps -> Alt -> TI (Context, Assumps, MonoType)
>   tiLetAssumps env as alt@(Alt [p] _) = tiPattern env as p >>= \(IR ctx as' (Ty _ mt)) -> return (ctx, as', mt)
>   tiLetAssumps env as (Alt (p:ps) _)  = do
>       bn       <- tiLetBindingName p
>       t        <- newTyVar KStar
>       return (S.empty, bn ~= mkPoly t, t)

    -- TODO: context reduction for let groups (see tiImpls)

>   tiLet :: Envs -> Assumps -> (Alt, MonoType) -> TI Context
>   tiLet env as (Alt [p] e, t)    = do
>       (ctx,t') <- tiExpr env as e
>       unify env t t'
>       return ctx
>   tiLet env as (Alt (p:ps) e, t) = do
>       bn       <- tiLetBindingName p
>       (ctx,t') <- tiEquation env as (Eq bn (Alt ps e))
>       unify env t t'
>       return ctx

    
>   tiLetGroup :: Infer [Alt] Assumps
>   tiLetGroup env as alts = do
>       rs <- mapM (tiLetAssumps env as) alts
>       let
>           ctx = S.unions [c | (c, _, _) <- rs]
>           as' = M.unions [a | (_, a, _) <- rs]
>           ts  = [t | (_, _, t) <- rs]
>           --ls  = [l | (_, _, _, l) <- rs] -- TODO: do something with ls 
>       ctxs <- mapM (tiLet env (as' `M.union` as)) (zip alts ts)    
>       return (ctx `S.union` S.unions ctxs, as' `M.union` as)
    
>   tiBindGroup :: Infer BindGroup Assumps
>   tiBindGroup env as (BG es iss ins) = do
>       let
>           as' = fromList $ map (\ex -> (explName ex, explType ex)) es
>       (ctx, as'') <- tiSeq tiImpls env (as' <> as) iss 
>       ctxs        <- mapM (tiExpl env (as'' <> as' <> as)) es
>       mapM (tiInsts env (as'' <> as' <> as)) ins
>       return (ctx `S.union` S.unions ctxs, as'' <> as') 

>   tiSeq :: Infer a Assumps -> Infer [a] Assumps
>   tiSeq f env as []       = return (S.empty, empty)
>   tiSeq f env as (xs:xss) = do
>       (ctx, as')   <- f env as xs
>       (ctx', as'') <- tiSeq f env (as' <> as) xss
>       return (ctx `S.union` ctx', as'' <> as')

    {----------------------------------------------------------------------}
    {-- External interface                                                -}
    {----------------------------------------------------------------------}
    
>   inferTypes :: Envs -> Assumps -> BindGroup -> Either TypeError Assumps
>   inferTypes env as bg = runTI $ fmap snd (tiBindGroup env as bg)
   
{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
