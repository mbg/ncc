{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

> module CodeGen.HsPretty (
>   hsLanguageExt,
>   hsModule,
>   hsImport,
>   hsQualImport,
>   hsDataHeader,
>   hsDataConstr,
>   hsDataConstrs,
>   hsTypeAlias,
>   hsClassHeader,
>   hsInstHeader,
>   hsTypeDecl,
>   hsEquation
> ) where

>   import Data.Char
>   import qualified Data.Map as M
>   import qualified Data.Set as S

>   import Cada.AST
>   import Cada.PrettyPrint

>   import TypeSystem.Kind
>   import TypeSystem.Types
>   import TypeSystem.PolyType
>   import TypeSystem.Environments

>   import Utility.PrettyPrint

>   hsComment :: ShowS -> ShowS
>   hsComment xs = 
>       showString "{- " . 
>       xs . 
>       showString " -}"

>   hsKindComment :: Kind -> ShowS
>   hsKindComment k = hsComment $
>       showString ": " .
>       ppKind False k

>   hsLanguageExt :: String -> ShowS
>   hsLanguageExt n = 
>       showString "{-# LANGUAGE " .
>       showString n .
>       showString " #-}"

>   hsModuleExport :: String -> ShowS
>   hsModuleExport m = 
>       ppTab .
>       showString "module " .
>       showString m

>   hsValueExport :: String -> ShowS
>   hsValueExport n = 
>       ppTab .
>       hsName n

>   hsTypeExport :: String -> ShowS
>   hsTypeExport t =
>       ppTab .
>       showString t .
>       showString "(..)"

>   hsExportGroup :: (String -> ShowS) -> [String] -> ShowS
>   hsExportGroup _ [] = id
>   hsExportGroup f es = 
>       ppDefsW f (showString ",\n") es .
>       showString ",\n"

>   hsModule :: String -> [String] -> [String] -> Envs -> ShowS
>   hsModule n ms vs envs =
>       showString "module " .
>       showString n .
>       showString " (\n" .
>       --hsExportGroup hsModuleExport ms .
>       hsExportGroup hsValueExport vs .
>       hsExportGroup hsTypeExport (M.keys (adtEnv envs)) .
>       hsExportGroup hsTypeExport (M.keys (alEnv envs)) .
>       hsExportGroup hsTypeExport (M.keys (clEnv envs)) .
>       showString "\n) where"

>   hsImport :: String -> ShowS
>   hsImport n = showString "import " . showString n

>   hsQualImport :: String -> String -> ShowS
>   hsQualImport n a = 
>       showString "import qualified " . 
>       showString n . 
>       showString " as " . 
>       showString a

    Dots are not allowed in variable names, so we replace 
    them with underscores.

>   hsFixChar :: Char -> Char
>   hsFixChar '.' = '_'
>   hsFixChar c   = c

    Operators may appear in the same positions as variables,
    but need to be surrounded by parenthesis, so we wrap them
    into some instead of trying to figure out where they should
    be placed.

>   hsName :: String -> ShowS
>   hsName n@(x:_)
>       | isAlpha x = showString (map hsFixChar n)
>       | otherwise = ppInParens $ showString n

>   hsOp :: String -> ShowS
>   hsOp n@(x:_)
>       | isAlpha x = showChar '`' . showString (map hsFixChar n) . showChar '`'
>       | otherwise = showString n

    Quantifiers don't need to be added to the surface syntax of
    Haskell (we don't use Rank2Types or RankNTypes), so we just
    ignore them and print the qualified monomorphic type instead.

>   hsPolyType :: PolyType -> ShowS
>   hsPolyType (ForAll _ qt) = ppQual qt

    Our internal representation of types doesn't contain a list
    of type parameters, so we must reconstruct it with the help
    of kinds or, in the case of type functions, the arity.

>   hsKindAsParams :: Kind -> ShowS
>   hsKindAsParams = hsArityAsParams . length . kindArgs

>   hsArityAsParams :: Int -> ShowS
>   hsArityAsParams n = let
>       vs = map TGen [0..n-1] in
>       ppDefsW (ppMonoType 0) ppSpace vs

    Generates the LHS or header of an ADT declaration. Use the first
    definition if GADTSyntax is available. This is only needed because
    the version of GHC on the lab PCs is too old.

   hsDataHeader :: String -> Kind -> ShowS
   hsDataHeader n k = 
       showString "data " . 
       showString n . 
       ppSpace .
       hsKindAsParams k .
       ppSpace .
       hsKindComment k .
       showString " where"

>   hsDataHeader :: String -> Kind -> ShowS
>   hsDataHeader n k = 
>       showString "data " . 
>       showString n . 
>       ppSpace .
>       hsKindAsParams k .
>       ppSpace .
>       hsKindComment k .
>       showString " = " 

    The type of a data constructor is given as a polytype,
    but if GADTSyntax is unavailable, we will need to print
    the parameter types as a space-separated list. Note that
    parameters types can only occur left of an arrow.

>   hsMonoTypeToParams :: MonoType -> ShowS
>   hsMonoTypeToParams (TApp (TApp f a) b)
>       | isArrow f = 
>           ppMonoType 10 a . ppSpace . 
>           hsMonoTypeToParams b
>   hsMonoTypeToParams _ = id

   hsDataConstr :: String -> PolyType -> ShowS
   hsDataConstr n pt = 
       ppTab .
       showString n .
       ppTyping .
       hsPolyType pt
       
>   hsDataConstr :: Bool -> String -> PolyType -> ShowS
>   hsDataConstr f n pt =
>       ppTab .
>       ppIf f (showString "  ") .
>       ppIf (not f) (showString "| ") .
>       showString n .
>       ppSpace .
>       hsMonoTypeToParams (mkMono pt) .
>       ppNewLine

>   hsDataConstrs :: [(String, PolyType)] -> ShowS 
>   hsDataConstrs [(n,pt)]    = hsDataConstr True n pt
>   hsDataConstrs ((n,pt):cs) = 
>       hsDataConstr True n pt .
>       ppDefs (uncurry (hsDataConstr False)) cs

>   hsTypeAlias :: String -> Kind -> Int -> PolyType -> ShowS
>   hsTypeAlias n k a pt = 
>       showString "type " .
>       showString n .
>       ppSpace .
>       hsArityAsParams a .
>       ppSpace .
>       hsKindComment k .
>       showString " = " .
>       hsPolyType pt

>   hsClassHeader :: String -> Context -> Kind -> ShowS
>   hsClassHeader n ctx k = 
>       showString "class " .
>       ppContext ctx .
>       showString n .
>       ppSpace .
>       hsKindAsParams k .
>       ppSpace .
>       hsKindComment k .
>       showString " where"

>   hsInstHeader :: String -> Context -> MonoType -> ShowS
>   hsInstHeader n ctx mt =
>       showString "instance " .
>       ppContext ctx .
>       showString n .
>       ppSpace .
>       ppMonoType 10 mt .
>       showString " where"

>   hsTypeDecl :: Int -> String -> PolyType -> ShowS
>   hsTypeDecl i n pt =
>       ppIndent i .
>       hsName n .
>       ppTyping .
>       hsPolyType pt

>   hsLiteral :: Int -> Literal -> ShowS
>   hsLiteral _ UnitLit      = ppInParens id
>   hsLiteral _ (StrLit xs)  = showString xs 
>   hsLiteral _ (IntLit xs)  = showString xs
>   hsLiteral i (PairLit es) = ppInParens $ ppDefsW (hsExpr' 0 i) (showChar ',') es 
>   hsLiteral i (ListLit es) = ppList $ ppDefsW (hsExpr' 0 i) (showChar ',') es

>   hsStmt :: Int -> Statement -> ShowS
>   hsStmt i (Statement e) = ppNewLine . ppIndent i . hsExpr' 0 i e 
>   hsStmt i (Bind p e)    = ppNewLine . ppIndent i . ppPattern p . showString " <- " . hsExpr' 0 i e 
>   hsStmt i (Getter p n)  = ppNewLine . ppIndent i . ppPattern p . showString " <- " . hsName (n ++ ".get") 
>   hsStmt i (Setter e n)  = ppNewLine . ppIndent i . hsName (n ++ ".set") . ppSpace . hsExpr' 10 i e 

>   hsDo :: Int -> Int -> [Statement] -> ShowS
>   hsDo p i xs = ppInOptParens (p>=5) $ showString "do" .  ppDefs (hsStmt (i+1)) xs 

>   hsLetBind :: Int -> Alt -> ShowS
>   hsLetBind i alt = ppNewLine . ppIndent i . hsAlt i alt

>   hsOption :: Int -> Option -> ShowS
>   hsOption i (Option p e) = 
>       ppNewLine . 
>       ppIndent i . 
>       ppPattern p .
>       showString " -> " .
>       hsExpr' 0 i e

>   hsExpr' :: Int -> Int -> Expr -> ShowS
>   hsExpr' p i (Var x)   = hsName x
>   hsExpr' p i (Ctr x)   = hsName x
>   hsExpr' p i (Lit l)   = hsLiteral i l
>   hsExpr' p i (Abs alt) = ppInOptParens (p>=5) $ 
>       showChar '\\' . 
>       ppDefsW ppPattern ppSpace (altPats alt) . 
>       showString " -> " . 
>       hsExpr' 0 i (altExpr alt)
>   hsExpr' p i (App l r) = ppInOptParens (p>=5) $ 
>       hsExpr' 0 i l . ppSpace . hsExpr' 5 i r
>   hsExpr' p i (InfixOp o l r) = ppInOptParens (p>=5) $ 
>       hsExpr' 0 i l . ppSpace . hsOp o . ppSpace . hsExpr' 0 i r 
>   hsExpr' p i (Let alts e)    = ppInOptParens (p>=5) $
>       showString "let" . 
>       ppDefs (hsLetBind (i+1)) alts . 
>       showString " in\n" . 
>       hsExpr p (i+2) e
>   hsExpr' p i (Cond c t f) =
>       showString "if " .
>       hsExpr' 0 i c .
>       showString " then\n" .
>       hsExpr 0 (i+1) t .
>       ppNewLine .
>       ppIndent i .
>       showString "else\n" . 
>       hsExpr 0 (i+1) f
>   hsExpr' p i (Case e opts)    = ppInOptParens (p>=5) $ 
>       showString "case " .
>       hsExpr' p i e .
>       showString " of" .
>       ppDefs (hsOption (i+1)) opts
>   hsExpr' p i (Do xs)         = hsDo p i xs
>   hsExpr' p i _               = showString "undefined {- not implemented -}"

>   hsExpr :: Int -> Int -> Expr -> ShowS
>   hsExpr p i e = ppIndent i . hsExpr' p i e

>   hsAlt :: Int -> Alt -> ShowS
>   hsAlt i (Alt ps e) =
>       ppDefsW ppPattern ppSpace ps .
>       showString " = " .
>       hsExpr' 0 i e
 
>   hsEquation :: Int -> Equation -> ShowS
>   hsEquation i (Eq n alt) =
>       ppIndent i .
>       hsName n .
>       ppSpace .
>       hsAlt i alt
 
{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
