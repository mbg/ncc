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
>   import qualified Data.Map as M
>   import qualified Data.Set as S
    
>   import Cada.AST
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

>   import Internal.NCC

>   import Utility.Set

    {----------------------------------------------------------------------}
    {-- Type Inference                                                    -}
    {----------------------------------------------------------------------}

>   type Infer e t = Envs -> Assumps -> e -> TI (Context, t)

    {----------------------------------------------------------------------}
    {-- Literals                                                          -}
    {----------------------------------------------------------------------}

>   tiLiteral :: Envs -> Assumps -> Literal -> TI (Context, MonoType)
>   tiLiteral env as UnitLit     = return (S.empty, unitType)
>   tiLiteral env as (StrLit _)  = return (S.empty, stringType)
>   tiLiteral env as (IntLit _)  = return (S.empty, intType)
>   tiLiteral env as (PairLit es) = do
>       tiExpr env as $ foldl App (Var "(,)") es
>   tiLiteral env as (ListLit es) = do
>       (ctx,ts)     <- tiExprs env as es
>       (ctx' :=> t) <- newInst listPolyType
>       mapM (unify env t) (map (TApp listType) ts)  
>       return (ctx `S.union` ctx', t) 
    
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
    
>   tiPatLiteral :: Envs -> Assumps -> Literal -> TI (Context, Assumps, MonoType)
>   tiPatLiteral env as UnitLit     = return (S.empty, empty, unitType)
>   tiPatLiteral env as (StrLit _)  = return (S.empty, empty, stringType)
>   tiPatLiteral env as (IntLit _)  = do
>       v <- newTyVar KStar
>       return (S.singleton (In "Num" v), empty, v)
>   tiPatLiteral env as (PairLit es) = do
>       (ctx,as',ts) <- tiPatExprs as es
>       t'           <- newTyVar KStar
>       pt           <- find "(,)" as
>       (ctx' :=> t) <- newInst pt 
>       unify env t (foldr mkFun t' ts) 
>       s <- getTheta
>       return (ctx `S.union` ctx', as', t') 
>   tiPatLiteral env as (ListLit es) = do
>       (ctx,as',ts) <- tiPatExprs as es
>       (ctx' :=> t) <- newInst listPolyType
>       s <- getTheta
>       mapM (unify env t) (map mkListType ts)  
>       return (ctx `S.union` ctx', as', t) 
    
>   tiPattern :: Envs -> Assumps -> Pattern -> TI (Context, Assumps, MonoType)
>   tiPattern _ _ Wildcard          = do
>       v <- newTyVar KStar
>       return (S.empty, empty, v)
>   tiPattern _ _ (VarPattern x)    = do 
>       v <- newTyVar KStar
>       return (S.empty, x ~= mkPoly v, v)
>   tiPattern env as (LitPattern l)   = tiPatLiteral env as l
>   tiPattern env as (CtrPattern f ps) = do
>       (ctx, as', ts) <- tiPatterns env as ps
>       t'             <- newTyVar KStar
>       pt             <- find f as
>       (ctx' :=> t)   <- newInst pt 
>       unify env t (foldr mkFun t' ts) 
>       return (ctx `S.union` ctx', as', t')

>   tiPatterns :: Envs -> Assumps -> [Pattern] -> TI (Context, Assumps, [MonoType])
>   tiPatterns env as ps = do
>       rs <- mapM (tiPattern env as) ps
>       let
>           ctx = S.unions [c | (c,_,_) <- rs]
>           as' = unions [a | (_,a,_) <- rs]
>           ts  = [t | (_,_,t) <- rs]
>       return (ctx, as', ts) 

    {----------------------------------------------------------------------}
    {-- Expressions                                                       -}
    {----------------------------------------------------------------------}

>   tiOption :: Envs -> Assumps -> MonoType -> MonoType -> Option -> TI Context
>   tiOption env as t rt opt = tiOption' env as t rt opt `inContext` 
>       \err -> OptionError opt err

>   tiOption' :: Envs -> Assumps -> MonoType -> MonoType -> Option -> TI Context
>   tiOption' env as t rt (Option p e) = do
>       (ctx, as', t') <- tiPattern env as p 
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
>       return (ctx, t)        
>   tiExpr' env as (Lit l) = tiLiteral env as l
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
>       pt'          <- find (n ++ ".get") as
>       (ntx :=> nt) <- newInst pt'
>       t            <- newTyVar KStar
>       unify env (nt `mkFun` t) bt
>       return (btx `S.union` ntx, t, p)
>   tiStmt' env as (Setter e n)  = do
>       pt           <- find ">>=" as
>       (btx :=> bt) <- newInst pt
>       (etx, et)    <- tiExpr env as (App (Var (n ++ ".set")) e)
>       t            <- newTyVar KStar
>       unify env (et `mkFun` t) bt
>       return (btx `S.union` etx, t, Wildcard)      

>   tiStmts :: Infer [Statement] MonoType
>   tiStmts env as [Statement e] = tiExpr' env as e
>   tiStmts env as [Setter e n]  = tiExpr' env as (App (Var (n ++ ".set")) e)
>   tiStmts env as (x:xs)        = do
>       (ctx,et,p)   <- tiStmt env as x
>       (ptx,as',pt) <- tiPattern env as p
>       (stx,st)     <- tiStmts env (as' <> as) xs
>       t            <- newTyVar KStar
>       unify env ((pt `mkFun` st) `mkFun` t) et
>       return (ctx `S.union` ptx `S.union` stx,t)
    
    {----------------------------------------------------------------------}
    {-- Equations                                                         -}
    {----------------------------------------------------------------------}
   
>   tiAlt :: Infer Alt MonoType
>   tiAlt env as (Alt ps e) = do
>       (ctx, as', ts) <- tiPatterns env as ps
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
>       EqsError (eqName $ head $ explEqs ex)
    
>   tiExpl' :: Envs -> Assumps -> Expl -> TI Context
>   tiExpl' env as (Expl n pt eqs) = do
>       (ctx :=> t) <- newInst pt
>       etx         <- tiEquations env as eqs t
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
>           simple (Impl n eqs) = any (null . altPats . eqAlt) eqs
   
>   tiImpls :: Infer [Impl] Assumps
>   tiImpls env as bs = do
>       ts  <- mapM (\_ -> newTyVar KStar) bs
>       ctx <- tiImpls' env (mkAssumps (map implName bs) (map mkPoly ts) <> as) (map implEqs bs) ts
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
>   tiLetAssumps env as alt@(Alt [p] _) = tiPattern env as p
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
