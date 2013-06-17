{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

This module contains helper functions for parse errors.

> module Cada.ParserError (
>   qualErrH,
>   duplicatesCheckH,
>   duplicatesCheckH',
>   typeFreeVarErr,
>   recAliasErrH,
>   dctrFreeVarErrH,
>   checkInstHead,
>   ppQualError,
>   --equationErrH,
>   ppTypeParamError,
>   ppDupFieldError,
>   ppDupFieldErrorS,
>   ppDupCtrError,
>   ppParserError
> ) where

    {----------------------------------------------------------------------}
    {-- Module Imports                                                    -}
    {----------------------------------------------------------------------}
    
>   import Control.Applicative
    
>   import qualified Data.Set as S
    
>   import Cada.Token
>   import Cada.Lexer
>   import Cada.ParserMonad
>   import Cada.AST
>   import Cada.PrettyPrint

>   import Utility.PrettyPrint
    
    {----------------------------------------------------------------------}
    {-- Error Handling                                                    -}
    {----------------------------------------------------------------------}
    
>   qualErrH :: Pos -> String -> Parser ()
>   qualErrH p n | isUnqualID n = return ()
>                | otherwise    = addErrorP p (ppQualError n)

>   elemL :: Eq a => Loc a -> [Loc a] -> Bool
>   elemL x xs = unL x `elem` unLs xs

>   filterL :: Eq a => (a -> Bool) -> [Loc a] -> [Loc a]
>   filterL p []     = []
>   filterL p (x:xs) 
>       | p (unL x) = x : filterL p xs
>       | otherwise = filterL p xs 

>   duplicatesCheckH :: (Eq a, Positioned a) => (a -> String) -> [a] -> Parser ()
>   duplicatesCheckH f []     = return ()
>   duplicatesCheckH f (x:xs) | x `elem` xs = do addErrorP (srcPos x) $ f x
>                                                duplicatesCheckH f $ filter (/= x) xs
>                             | otherwise   = do duplicatesCheckH f xs

>   duplicatesCheckH' :: Eq a => (a -> String) -> [Loc a] -> Parser ()
>   duplicatesCheckH' f []     = return ()
>   duplicatesCheckH' f (x:xs) | x `elemL` xs = do addErrorP (srcPos x) $ f $ unL x
>                                                  duplicatesCheckH' f $ filterL (/= unL x) xs
>                              | otherwise    = do duplicatesCheckH' f xs

>   tyVarOccurs :: SType -> TypeParam -> Bool
>   tyVarOccurs (STyVar n) (TyP n' _) = n == n'
>   tyVarOccurs _          _          = False

>   checkVars :: String -> [TypeParam] -> [SType] -> Parser ()
>   checkVars n ps []     = return ()
>   checkVars n ps (v:vs) 
>       | any (tyVarOccurs v) ps = do checkVars n ps vs
>       | otherwise              = do p <- FilePos <$> getPosP
>                                     addErrorP p (ppTypeVarDeclError n (tyVarName v))
>                                     checkVars n ps vs

>   typeFreeVarErr :: String -> [TypeParam] -> SType -> Parser ()
>   typeFreeVarErr n ps t = checkVars n ps (S.toList $ freeTyVars t) 

>   recAliasErrH :: Pos -> String -> SType -> Parser ()
>   recAliasErrH p n t
>       | occursIn n t = addErrorP p (ppRecAliasError n)
>       | otherwise    = return ()
    
>   dctrFreeVarErrH :: String -> [TypeParam] -> [Loc DataConstructor] -> Parser ()
>   dctrFreeVarErrH n ps cs = checkVars n ps $ S.toList $ freeTyVars $ unLs cs 

    Checks that all equations have the same number of parameters.

   hasEqCount :: Int -> DefDeclaration -> Bool
   hasEqCount n (EqDecl {eqDeclPat = ps}) = n == length ps
    
   findEquationArgC :: AlexPosn -> String -> [DefDeclaration] -> Parser (Maybe (Int, [DefDeclaration]))
   findEquationArgC p n []                             = addErrorP p (ppNoEquationError n) >> return Nothing
   findEquationArgC _ _ (EqDecl {eqDeclPat = ps} : ds) = return $ Just (length ps, ds)
    
   equationErrH :: AlexPosn -> String -> [DefDeclaration] -> Parser ()
   equationErrH p n ds = do
       r <- findEquationArgC p n ds
       case r of
           Nothing         -> return ()
           (Just (c, ds')) -> if all (hasEqCount c) ds'
                              then return ()
                              else addErrorP p (ppEquationsErr n)
    
    {----------------------------------------------------------------------}
    {-- Instances                                                         -}
    {----------------------------------------------------------------------}
    
>   checkHead :: S.Set String -> SType -> Bool
>   checkHead vs (STyCtr v)            = True
>   checkHead vs (STyList (STyVar _))  = True
>   checkHead vs (STyTuple ps)         = True -- TODO: check vars
>   checkHead vs (STyApp x (STyVar v))
>       | S.notMember v vs             = checkHead (S.insert v vs) x
>   checkHead vs _                     = False
    
>   checkInstHead :: Pos -> String -> SType -> Parser ()
>   checkInstHead p cls st
>       | checkHead S.empty st = return ()
>       | otherwise            = addErrorP p (ppInstHeadErr cls st) 

>   ppInstHeadErr :: String -> SType -> String
>   ppInstHeadErr cls st = ppDo $
>       showString "Illegal instance declaration for " .
>       ppId (showString cls . ppSpace . ppType st) .
>       showString ":\n\n\t" .
>       showString "All instance types must be of the form T a0 .. an\n\t" .
>       showString "where a0 .. an are distinct type variables."
    
    {----------------------------------------------------------------------}
    {-- Pretty Printing                                                   -}
    {----------------------------------------------------------------------}
        
>   ppQualError :: String -> String
>   ppQualError n = ppDo $ 
>       showString "Qualified identifier " . 
>       ppId (showString n) . 
>       showString " is not permitted in this context."

>   ppTypeParamError :: String -> String
>   ppTypeParamError n = ppDo $
>       showString "Type parameter " .
>       ppId (showString n) .
>       showString " is declared more than once. " .
>       showString "All type parameters must have unique names."

>   ppDupFieldError :: String -> String -> String
>   ppDupFieldError c f = ppDo $
>       showString "More than one field named " .
>       ppId (showString f) .
>       showString " exists in the definition of data constructor " .
>       ppId (showString c) .
>       showString "."

>   ppDupFieldErrorS :: String -> String -> String
>   ppDupFieldErrorS t f = ppDo $
>       showString "More than one field named `" .
>       showString f .
>       showString "' exists in the definition of data type `" .
>       showString t .
>       showString "'."

>   ppDupCtrError :: String -> String -> String
>   ppDupCtrError t n = ppDo $
>       showString "More than one constructor named `" .
>       showString n .
>       showString "' exists in the definition of data type `" .
>       showString t .
>       showString "'."

>   ppTypeVarDeclError :: String -> String -> String
>   ppTypeVarDeclError n v = ppDo $
>       showString "Type variable `" .
>       showString v .
>       showString "' must be declared as a parameter of type `" .
>       showString n .
>       showString "'"

>   ppRecAliasError :: String -> String
>   ppRecAliasError n = ppDo $
>       showString "Type alias " .
>       ppId (showString n) .
>       showString " is defined in terms of itself,\n\t" .
>       showString "but type aliases may not be recursive."
    
    Pretty-prints a generic parser error.
    
>   ppParserError :: Token -> String
>   ppParserError t = ppDo $ 
>       showString "Unexpected lexical token\n\t\t" . 
>       showString (show t) .
>       showString "\n\tin the current context."

>   ppNoEquationError :: String -> String
>   ppNoEquationError n = ppDo $
>       showString "The definition for `" .
>       showString n .
>       showString "' has no equations."

>   ppEquationsErr :: String -> String
>   ppEquationsErr n = ppDo $
>       showString "The equations for `" .
>       showString n .
>       showString "' have an inconsistent number of parameters."

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
