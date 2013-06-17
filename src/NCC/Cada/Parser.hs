{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

module Cada.Parser (
    parseModule,
    runCadaParser,
    runExprParser,
    hasErrors,
    unA
) where

{--------------------------------------------------------------------------}
{-- Module Imports                                                        -}
{--------------------------------------------------------------------------}

import Control.Applicative

import Utility.StateM

import Cada.Token
import Cada.Location
import Cada.ParserMonad
import Cada.ParserError
import Cada.ParserConstr
import Cada.Lexer
import Cada.AST
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts

-- parser produced by Happy Version 1.18.10

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn5 :: (Loc String) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> (Loc String)
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: (Loc String) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> (Loc String)
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (Literal) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (Literal)
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (Literal) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (Literal)
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: (Pattern) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> (Pattern)
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: ([Pattern]) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> ([Pattern])
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: (Accum Pattern) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> (Accum Pattern)
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: (Accum Pattern) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> (Accum Pattern)
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: (Pattern) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> (Pattern)
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: ([Pattern]) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> ([Pattern])
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: (Accum Pattern) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> (Accum Pattern)
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: ([SType]) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> ([SType])
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (Accum SType) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (Accum SType)
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (TypeParam) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (TypeParam)
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: ([TypeParam]) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> ([TypeParam])
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (Accum TypeParam) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (Accum TypeParam)
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: (SType) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> (SType)
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: (SType) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> (SType)
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: (SType) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> (SType)
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: (SType) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> (SType)
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: ([TypeConstraint]) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> ([TypeConstraint])
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: (Loc String) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> (Loc String)
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: (Loc [String]) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> (Loc [String])
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: (Accum (Loc String)) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> (Accum (Loc String))
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: (Loc DecType) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> (Loc DecType)
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: ([Loc DecType]) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> ([Loc DecType])
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: (Accum (Loc DecType)) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> (Accum (Loc DecType))
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: (STypeClass Loc) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> (STypeClass Loc)
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: ([InstDef Loc]) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> ([InstDef Loc])
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: (Accum (InstDef Loc)) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> (Accum (InstDef Loc))
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: (InstDef Loc) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> (InstDef Loc)
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: (SInstance Loc) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> (SInstance Loc)
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: ([Loc DataConstructor]) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> ([Loc DataConstructor])
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: (DataField) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> (DataField)
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: ([DataField]) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> ([DataField])
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: (Accum DataField) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> (Accum DataField)
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: ([Loc DataConstructor]) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> ([Loc DataConstructor])
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: (LocP Definition) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> (LocP Definition)
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: (Maybe SType) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> (Maybe SType)
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: (LocP Definition) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> (LocP Definition)
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: (Alt) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> (Alt)
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: ([Alt]) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> ([Alt])
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyIn47 :: (Accum Alt) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> (Accum Alt)
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyIn48 :: (Option) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> (Option)
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
happyIn49 :: ([Option]) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> ([Option])
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
happyIn50 :: (Accum Option) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> (Accum Option)
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
happyIn51 :: ([Statement]) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> ([Statement])
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
happyIn52 :: (Accum Statement) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> (Accum Statement)
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
happyIn53 :: (Statement) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> (Statement)
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
happyIn54 :: (Expr) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> (Expr)
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
happyIn55 :: (Expr) -> (HappyAbsSyn )
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> (Expr)
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
happyIn56 :: (Expr) -> (HappyAbsSyn )
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> (Expr)
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
happyIn57 :: (Expr) -> (HappyAbsSyn )
happyIn57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> (Expr)
happyOut57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut57 #-}
happyIn58 :: ([Expr]) -> (HappyAbsSyn )
happyIn58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> ([Expr])
happyOut58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut58 #-}
happyIn59 :: (Accum Expr) -> (HappyAbsSyn )
happyIn59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn ) -> (Accum Expr)
happyOut59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut59 #-}
happyIn60 :: (Accum Expr) -> (HappyAbsSyn )
happyIn60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn60 #-}
happyOut60 :: (HappyAbsSyn ) -> (Accum Expr)
happyOut60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut60 #-}
happyIn61 :: (Loc Equation) -> (HappyAbsSyn )
happyIn61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn61 #-}
happyOut61 :: (HappyAbsSyn ) -> (Loc Equation)
happyOut61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut61 #-}
happyIn62 :: (LocP Module) -> (HappyAbsSyn )
happyIn62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn62 #-}
happyOut62 :: (HappyAbsSyn ) -> (LocP Module)
happyOut62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut62 #-}
happyIn63 :: ([LocP Definition]) -> (HappyAbsSyn )
happyIn63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn63 #-}
happyOut63 :: (HappyAbsSyn ) -> ([LocP Definition])
happyOut63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut63 #-}
happyIn64 :: (Accum (LocP Definition)) -> (HappyAbsSyn )
happyIn64 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn64 #-}
happyOut64 :: (HappyAbsSyn ) -> (Accum (LocP Definition))
happyOut64 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut64 #-}
happyIn65 :: (LocP Definition) -> (HappyAbsSyn )
happyIn65 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn65 #-}
happyOut65 :: (HappyAbsSyn ) -> (LocP Definition)
happyOut65 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut65 #-}
happyInTok :: (TokenP) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (TokenP)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x25\x02\x35\x01\xaa\x00\x00\x00\x29\x02\x00\x00\x00\x00\x00\x00\x00\x00\x23\x02\x41\x01\x00\x00\x00\x00\x1c\x01\x1c\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x35\x01\x35\x01\x00\x00\x23\x02\x27\x02\x39\x02\x36\x02\x03\x01\x00\x00\x03\x02\x35\x02\xd7\x00\x00\x00\xfa\x00\x2c\x02\x00\x00\xaa\x00\x34\x02\x00\x00\x2f\x02\x31\x02\x33\x02\x35\x01\x00\x00\x2b\x02\x00\x00\x00\x00\x10\x01\x4e\x01\x00\x00\x00\x00\x35\x01\x00\x00\x35\x01\x35\x01\x32\x02\x35\x01\x00\x00\x35\x01\x00\x00\x00\x00\xed\x00\x00\x00\x00\x00\x00\x00\x35\x01\x00\x00\x2e\x02\xae\x00\xaa\x00\x00\x00\x2a\x02\x00\x00\x24\x02\x22\x02\xed\x00\x00\x00\x00\x00\x00\x00\xaa\x00\x00\x00\x6d\x01\xe8\x00\xaa\x00\x00\x00\xaa\x00\x35\x01\x1e\x02\x00\x00\x1d\x02\x21\x02\x20\x02\x7e\x01\x18\x02\x00\x00\x1f\x02\xe8\x00\x35\x01\x00\x00\x1b\x02\xe3\x00\xe3\x00\x00\x00\x17\x02\x1b\x00\x1c\x02\x16\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc6\x00\x06\x02\x0d\x00\x15\x02\x14\x02\x0d\x00\x13\x02\x12\x02\x11\x02\x0a\x02\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x02\x6b\x01\x00\x00\x00\x00\x0b\x02\x00\x00\x9a\x01\x93\x01\x00\x00\x00\x00\x19\x02\x00\x00\x63\x01\x07\x02\x00\x00\x0f\x02\x07\x00\x0d\x00\x0e\x02\x00\x00\x00\x00\x00\x00\x00\x00\xaa\x00\x00\x00\x00\x00\x35\x01\x00\x00\x00\x00\x00\x00\x79\x01\x0d\x02\x35\x01\xd9\x01\x88\x01\xc7\x01\x00\x00\xba\x01\x00\x00\x00\x00\x0d\x00\x00\x00\x00\x00\xc8\x01\x9c\x01\xab\x01\xb0\x01\x9d\x01\x00\x00\x0a\x00\x00\x00\xa4\x01\x0d\x00\x00\x00\x00\x00\x9b\x01\x8f\x01\x8b\x01\x0d\x00\x8c\x01\x0d\x00\x76\x01\x84\x01\xcd\x00\x59\x01\x00\x00\x00\x00\x0d\x00\x0d\x00\x00\x00\x82\x01\x6c\x01\x00\x00\x00\x00\x65\x01\x70\x01\x07\x00\x23\x01\x0d\x00\x00\x00\x72\x01\x35\x01\x00\x00\x67\x01\x00\x00\x68\x01\x00\x00\x00\x00\x00\x00\x00\x00\x66\x01\x47\x01\x5f\x01\x08\x00\x49\x01\x00\x00\x00\x00\x46\x01\x40\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1d\x01\x0d\x00\x00\x00\x0f\x01\x00\x00\x1f\x01\xf4\x00\xf7\x00\x0d\x00\x00\x00\x00\x00\x00\x00\xdb\x00\x00\x00\xef\x00\xd9\x00\x00\x00\xe4\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x00\x00\x00\x00\xdd\x00\x00\x00\x00\x00\xd3\x00\x00\x00\x00\x00\xbb\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x8e\x00\x7a\x00\xcc\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x00\x00\xe7\x01\x17\x00\x0b\x00\x09\x02\x00\x00\x00\x00\x00\x00\x00\x00\x74\x00\x6e\x00\x90\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa9\x01\x00\x00\x00\x00\x00\x00\x10\x02\x00\x00\x10\x02\x00\x00\x00\x00\x10\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x02\x1d\x00\x00\x00\x00\x00\x6a\x00\x00\x00\x77\x00\x64\x00\x00\x00\x60\x00\xfd\xff\x5c\x00\x00\x00\x00\x00\xd3\x01\x00\x00\x00\x00\x00\x00\x51\x00\xd4\x01\x00\x00\xff\xff\x0c\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcc\x01\x00\x00\x01\x02\x00\x00\x08\x02\x00\x00\x08\x02\x84\x00\x08\x02\x00\x00\x08\x02\x4d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x02\x00\x00\x00\x00\x00\x00\x43\x00\x41\x00\x00\x00\x00\x00\xbc\x00\xb9\x00\x00\x00\x00\x00\xfd\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc5\x01\x00\x00\xa7\x01\x00\x00\x00\x00\xa2\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf6\x01\xf4\x01\xf2\x01\x00\x00\xaa\x01\x00\x00\x00\x00\x00\x00\x00\x00\xe6\x01\xe2\x01\x00\x00\x00\x00\x00\x00\x96\x01\x3e\x01\x00\x00\x00\x00\x00\x00\xa2\x00\xcd\x01\x00\x00\x25\x01\x00\x00\x00\x00\x00\x00\x43\x01\x00\x00\x00\x00\x37\x00\x00\x00\x00\x00\x00\x00\x43\x01\x00\x00\x29\x00\x00\x00\x2a\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xec\x01\x00\x00\x11\x01\x00\x00\xb0\x00\x95\x00\x00\x00\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\xe9\x01\xee\x00\x00\x00\x00\x00\x55\x00\x00\x00\xde\x01\x00\x00\x2e\x01\x00\x00\x00\x00\x1a\x00\xe2\x00\x00\x00\x00\x00\xb3\x01\xda\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xeb\x00\x00\x00\xd1\x01\xe2\x00\xd2\x01\x00\x00\xea\x00\x02\x00\x00\x00\xea\x00\x00\x00\x00\x00\xbf\x00\x00\x00\x00\x00\x00\x00\x00\x00\x57\x00\x00\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x91\x00\x42\x00\x00\x00\x00\x00\x00\x00\x4e\x00\x00\x00\x80\x01\x00\x00\x00\x00\x3e\x00\xe6\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x79\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe4\xff\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\xfd\xff\x00\x00\xfa\xff\xfb\xff\xf6\xff\x8e\xff\x00\x00\x94\xff\x92\xff\xa0\xff\x8b\xff\x8b\xff\xe3\xff\x91\xff\x90\xff\xf8\xff\xf7\xff\x00\x00\x00\x00\xe3\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe5\xff\xa7\xff\x00\x00\xa8\xff\x00\x00\x9a\xff\x00\x00\x00\x00\x89\xff\x88\xff\x00\x00\x8c\xff\x8a\xff\x00\x00\x00\x00\xa1\xff\x93\xff\x00\x00\xfc\xff\x9f\xff\x00\x00\x94\xff\x8f\xff\x8d\xff\x00\x00\xf9\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe3\xff\x00\x00\xf2\xff\xe4\xff\xee\xff\xf5\xff\xf4\xff\xf1\xff\x00\x00\x82\xff\x00\x00\x83\xff\xa9\xff\xe7\xff\x00\x00\xf0\xff\xed\xff\xec\xff\xee\xff\xea\xff\xe3\xff\xe6\xff\x95\xff\xa6\xff\x00\x00\x00\x00\x98\xff\x99\xff\x87\xff\x00\x00\x00\x00\x9e\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa3\xff\x00\x00\xa4\xff\x00\x00\xe9\xff\x00\x00\x00\x00\x00\x00\xf3\xff\x00\x00\xe3\xff\x00\x00\xcb\xff\x7a\xff\x7c\xff\x7b\xff\x79\xff\x81\xff\xee\xff\xcd\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x84\xff\xdd\xff\xdd\xff\xdd\xff\x00\x00\xce\xff\xd8\xff\xd1\xff\x00\x00\x7d\xff\x00\x00\x00\x00\xd7\xff\xcf\xff\x00\x00\xdd\xff\xce\xff\x00\x00\x7e\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe3\xff\xeb\xff\xef\xff\xe8\xff\x96\xff\xa2\xff\x97\xff\x00\x00\x9d\xff\x9c\xff\x9b\xff\x00\x00\x00\x00\x00\x00\x00\x00\xda\xff\x00\x00\xc9\xff\x00\x00\xcd\xff\xcc\xff\x00\x00\xd9\xff\xc5\xff\x00\x00\xde\xff\x00\x00\x00\x00\xda\xff\xd4\xff\x00\x00\xd0\xff\x00\x00\x00\x00\xc0\xff\x80\xff\x00\x00\xac\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc1\xff\x00\x00\xd3\xff\xd2\xff\x00\x00\x00\x00\xd5\xff\x00\x00\xb9\xff\xdc\xff\xdf\xff\xb6\xff\x00\x00\xc6\xff\x00\x00\x00\x00\xc7\xff\x00\x00\x00\x00\xa5\xff\x00\x00\x86\xff\x00\x00\xc5\xff\xca\xff\xc4\xff\xc2\xff\x00\x00\xb7\xff\x00\x00\xb4\xff\x00\x00\xb0\xff\xdb\xff\x00\x00\xe2\xff\xe1\xff\xc0\xff\xbe\xff\xbf\xff\xbd\xff\xbb\xff\x00\x00\xab\xff\xb6\xff\x00\x00\x7f\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd6\xff\xba\xff\xb6\xff\x00\x00\xae\xff\x00\x00\x00\x00\xaf\xff\x00\x00\xc8\xff\x85\xff\xc3\xff\x00\x00\xb5\xff\xb2\xff\x00\x00\xe0\xff\xbc\xff\x00\x00\xaa\xff\xad\xff\xb1\xff\xb8\xff\xb3\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x03\x00\x04\x00\x02\x00\x03\x00\x09\x00\x0a\x00\x24\x00\x01\x00\x24\x00\x04\x00\x01\x00\x02\x00\x03\x00\x05\x00\x08\x00\x04\x00\x08\x00\x06\x00\x15\x00\x16\x00\x17\x00\x18\x00\x01\x00\x02\x00\x03\x00\x14\x00\x02\x00\x03\x00\x04\x00\x02\x00\x03\x00\x14\x00\x15\x00\x08\x00\x25\x00\x28\x00\x27\x00\x02\x00\x03\x00\x20\x00\x0f\x00\x02\x00\x03\x00\x1c\x00\x1d\x00\x15\x00\x16\x00\x17\x00\x18\x00\x31\x00\x32\x00\x33\x00\x34\x00\x38\x00\x1e\x00\x02\x00\x03\x00\x3c\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x02\x00\x03\x00\x02\x00\x03\x00\x04\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x02\x00\x03\x00\x34\x00\x38\x00\x02\x00\x03\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x31\x00\x32\x00\x33\x00\x34\x00\x02\x00\x03\x00\x22\x00\x23\x00\x02\x00\x03\x00\x22\x00\x23\x00\x02\x00\x03\x00\x31\x00\x32\x00\x33\x00\x34\x00\x02\x00\x03\x00\x2b\x00\x21\x00\x02\x00\x03\x00\x31\x00\x32\x00\x33\x00\x34\x00\x02\x00\x03\x00\x21\x00\x02\x00\x03\x00\x26\x00\x02\x00\x03\x00\x31\x00\x32\x00\x33\x00\x34\x00\x31\x00\x32\x00\x33\x00\x34\x00\x02\x00\x03\x00\x04\x00\x10\x00\x11\x00\x12\x00\x13\x00\x31\x00\x32\x00\x33\x00\x34\x00\x31\x00\x32\x00\x33\x00\x34\x00\x31\x00\x32\x00\x33\x00\x34\x00\x09\x00\x0a\x00\x31\x00\x32\x00\x33\x00\x34\x00\x31\x00\x32\x00\x33\x00\x34\x00\x12\x00\x13\x00\x31\x00\x32\x00\x33\x00\x34\x00\x32\x00\x33\x00\x34\x00\x32\x00\x33\x00\x34\x00\x2b\x00\x2c\x00\x2d\x00\x04\x00\x09\x00\x06\x00\x20\x00\x0c\x00\x15\x00\x28\x00\x29\x00\x2a\x00\x02\x00\x03\x00\x0d\x00\x02\x00\x03\x00\x16\x00\x08\x00\x14\x00\x15\x00\x08\x00\x17\x00\x18\x00\x39\x00\x1a\x00\x1b\x00\x04\x00\x1d\x00\x06\x00\x01\x00\x20\x00\x21\x00\x15\x00\x04\x00\x24\x00\x06\x00\x26\x00\x02\x00\x28\x00\x29\x00\x19\x00\x1a\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x02\x00\x09\x00\x14\x00\x15\x00\x0c\x00\x17\x00\x18\x00\x02\x00\x04\x00\x0f\x00\x06\x00\x00\x00\x01\x00\x04\x00\x16\x00\x06\x00\x29\x00\x15\x00\x04\x00\x03\x00\x06\x00\x12\x00\x13\x00\x29\x00\x14\x00\x15\x00\x02\x00\x17\x00\x18\x00\x14\x00\x15\x00\x27\x00\x17\x00\x18\x00\x14\x00\x15\x00\x09\x00\x17\x00\x18\x00\x0c\x00\x04\x00\x14\x00\x06\x00\x1c\x00\x1d\x00\x29\x00\x22\x00\x23\x00\x24\x00\x16\x00\x29\x00\x03\x00\x03\x00\x12\x00\x13\x00\x29\x00\x14\x00\x15\x00\x09\x00\x17\x00\x18\x00\x0c\x00\x01\x00\x01\x00\x25\x00\x04\x00\x02\x00\x06\x00\x13\x00\x01\x00\x09\x00\x16\x00\x04\x00\x0c\x00\x06\x00\x19\x00\x1a\x00\x29\x00\x11\x00\x09\x00\x0a\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x01\x00\x14\x00\x15\x00\x04\x00\x1e\x00\x06\x00\x12\x00\x13\x00\x22\x00\x11\x00\x12\x00\x13\x00\x01\x00\x00\x00\x01\x00\x04\x00\x11\x00\x06\x00\x08\x00\x14\x00\x15\x00\x05\x00\x17\x00\x18\x00\x19\x00\x01\x00\x12\x00\x13\x00\x04\x00\x1e\x00\x06\x00\x14\x00\x15\x00\x22\x00\x17\x00\x18\x00\x01\x00\x14\x00\x0e\x00\x04\x00\x15\x00\x06\x00\x12\x00\x02\x00\x14\x00\x15\x00\x01\x00\x17\x00\x18\x00\x04\x00\x02\x00\x06\x00\x03\x00\x03\x00\x01\x00\x14\x00\x15\x00\x04\x00\x09\x00\x06\x00\x02\x00\x0c\x00\x08\x00\x03\x00\x09\x00\x14\x00\x15\x00\x0c\x00\x15\x00\x09\x00\x03\x00\x16\x00\x0c\x00\x14\x00\x15\x00\x03\x00\x09\x00\x16\x00\x02\x00\x0c\x00\x02\x00\x09\x00\x16\x00\x1c\x00\x0c\x00\x15\x00\x04\x00\x01\x00\x06\x00\x16\x00\x10\x00\x11\x00\x12\x00\x13\x00\x16\x00\x0d\x00\x0b\x00\x04\x00\x10\x00\x06\x00\x07\x00\x0c\x00\x14\x00\x15\x00\x04\x00\x05\x00\x06\x00\x04\x00\x08\x00\x06\x00\x0e\x00\x0f\x00\x0b\x00\x14\x00\x15\x00\x05\x00\x0d\x00\x02\x00\x03\x00\x04\x00\x14\x00\x15\x00\x14\x00\x14\x00\x15\x00\x11\x00\x12\x00\x13\x00\x14\x00\x07\x00\x11\x00\x12\x00\x13\x00\x14\x00\x12\x00\x13\x00\x0b\x00\x0c\x00\x15\x00\x1f\x00\x1b\x00\x10\x00\x11\x00\x12\x00\x13\x00\x02\x00\x03\x00\x01\x00\x05\x00\x06\x00\x07\x00\x08\x00\x02\x00\x03\x00\x16\x00\x05\x00\x06\x00\x07\x00\x08\x00\x02\x00\x03\x00\x10\x00\x05\x00\x06\x00\x07\x00\x08\x00\x03\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x10\x00\x11\x00\x12\x00\x13\x00\x15\x00\x16\x00\x17\x00\x18\x00\x10\x00\x11\x00\x12\x00\x13\x00\x10\x00\x11\x00\x12\x00\x13\x00\x10\x00\x11\x00\x12\x00\x13\x00\x10\x00\x11\x00\x12\x00\x13\x00\x11\x00\x12\x00\x13\x00\x11\x00\x12\x00\x13\x00\x0e\x00\x0f\x00\x0e\x00\x0f\x00\x0e\x00\x0f\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x3a\x00\x3b\x00\x00\x00\x01\x00\x09\x00\x0a\x00\x05\x00\x2e\x00\x2f\x00\x10\x00\x0b\x00\x0b\x00\x01\x00\x10\x00\x16\x00\x03\x00\x08\x00\x15\x00\x05\x00\x02\x00\x1f\x00\x03\x00\x03\x00\x0d\x00\x15\x00\x15\x00\x15\x00\x15\x00\x15\x00\x0f\x00\x08\x00\x16\x00\x0c\x00\x05\x00\x02\x00\x14\x00\x14\x00\x01\x00\x09\x00\x02\x00\x05\x00\x08\x00\x03\x00\x0d\x00\x01\x00\x07\x00\x15\x00\x14\x00\xff\xff\xff\xff\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x23\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x07\x00\x3b\x00\x67\x00\x07\x00\x08\x00\x1a\x00\x1b\x00\x07\x01\xf3\x00\xfe\x00\xa4\x00\x23\x00\x07\x00\x08\x00\xc1\x00\xf4\x00\x85\x00\xc2\x00\x86\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x23\x00\x07\x00\x08\x00\xa5\x00\x07\x00\x3b\x00\x67\x00\x07\x00\x08\x00\x87\x00\x88\x00\xca\xff\x6c\x00\x50\x00\x6d\x00\x07\x00\x08\x00\xf1\x00\xca\xff\x07\x00\x08\x00\xee\x00\xbd\x00\x68\x00\x69\x00\x6a\x00\xe3\x00\xd1\x00\x20\x00\x0a\x00\x0b\x00\x6e\x00\xe4\x00\x07\x00\x08\x00\x6f\x00\x24\x00\x20\x00\x0a\x00\x0b\x00\x25\x00\x26\x00\x27\x00\x07\x00\x08\x00\x07\x00\x3b\x00\x5d\x00\x24\x00\x20\x00\x0a\x00\x0b\x00\x28\x00\x26\x00\x27\x00\x07\x00\x08\x00\x2b\x00\xe5\x00\x07\x00\x08\x00\x2e\x00\x2f\x00\x20\x00\x30\x00\x0b\x00\xce\x00\x20\x00\x0a\x00\x0b\x00\x07\x00\x08\x00\xff\x00\xd9\x00\x07\x00\x08\x00\xec\x00\xd9\x00\x07\x00\x08\x00\x9c\x00\x20\x00\x0a\x00\x0b\x00\x07\x00\x08\x00\x96\x00\x02\x01\x07\x00\x08\x00\x95\x00\x20\x00\x0a\x00\x0b\x00\x07\x00\x08\x00\xf5\x00\x07\x00\x08\x00\xb9\x00\x07\x00\x08\x00\x5c\x00\x20\x00\x0a\x00\x0b\x00\x45\x00\x20\x00\x0a\x00\x0b\x00\x07\x00\x3b\x00\x5d\x00\x06\x01\xad\x00\x80\x00\x81\x00\x4f\x00\x20\x00\x0a\x00\x0b\x00\x51\x00\x20\x00\x0a\x00\x0b\x00\x53\x00\x20\x00\x0a\x00\x0b\x00\x1a\x00\x1b\x00\x55\x00\x20\x00\x0a\x00\x0b\x00\x1f\x00\x20\x00\x0a\x00\x0b\x00\xa7\x00\x81\x00\x21\x00\x20\x00\x0a\x00\x0b\x00\x54\x00\x0a\x00\x0b\x00\x09\x00\x0a\x00\x0b\x00\x5e\x00\x5f\x00\x60\x00\x71\x00\x05\x00\x0f\x00\xc4\x00\x06\x00\xa2\x00\x1c\x00\x1d\x00\x1e\x00\x07\x00\x46\x00\xc6\x00\x07\x00\x46\x00\x07\x00\x92\x00\x72\x00\x40\x00\x93\x00\x13\x00\x14\x00\x17\x00\x73\x00\x74\x00\x4c\x00\x75\x00\x0f\x00\x03\x00\x76\x00\x77\x00\xdc\x00\x71\x00\x78\x00\x0f\x00\x79\x00\x05\x01\x7a\x00\x41\x00\xf8\x00\xca\x00\x4d\x00\x4e\x00\x8e\x00\x13\x00\x14\x00\x06\x01\x05\x00\x72\x00\x40\x00\x06\x00\x13\x00\x14\x00\xfc\x00\x4c\x00\xfd\x00\x0f\x00\x35\x00\x03\x00\x3e\x00\x07\x00\x0f\x00\x4f\x00\xdc\x00\x4c\x00\xfe\x00\x0f\x00\xa7\x00\x81\x00\x41\x00\x4d\x00\x4e\x00\x02\x01\x13\x00\x14\x00\x3f\x00\x40\x00\x39\x00\x13\x00\x14\x00\x4d\x00\x4e\x00\x05\x00\x13\x00\x14\x00\x06\x00\x3e\x00\xf7\x00\x0f\x00\xbc\x00\xbd\x00\x4f\x00\xd8\x00\xd9\x00\xda\x00\x07\x00\x41\x00\xec\x00\x59\x00\xa7\x00\x81\x00\x4f\x00\x3f\x00\x40\x00\x05\x00\x13\x00\x14\x00\x06\x00\x0d\x00\xee\x00\x38\x00\x0e\x00\x04\x01\x0f\x00\x5a\x00\xd5\x00\x05\x00\x07\x00\x85\x00\x06\x00\x86\x00\xc9\x00\xca\x00\x41\x00\x10\x00\x9d\x00\x1b\x00\x11\x00\x12\x00\x07\x00\x13\x00\x14\x00\x15\x00\x0d\x00\x87\x00\x88\x00\x0e\x00\x16\x00\x0f\x00\xa7\x00\x81\x00\x17\x00\xe8\x00\x80\x00\x81\x00\x0d\x00\x35\x00\x03\x00\x0e\x00\x10\x00\x0f\x00\xf0\x00\x11\x00\x12\x00\xf1\x00\x13\x00\x14\x00\x15\x00\x0d\x00\xa7\x00\x81\x00\x0e\x00\x16\x00\x0f\x00\x11\x00\x12\x00\x17\x00\x13\x00\x14\x00\xe3\x00\xf7\x00\x57\x00\x85\x00\xc6\x00\x86\x00\x58\x00\xf5\x00\x11\x00\x12\x00\xa9\x00\x13\x00\x14\x00\x85\x00\xf8\x00\x86\x00\xfb\x00\xfa\x00\xb4\x00\x87\x00\x88\x00\x85\x00\x05\x00\x86\x00\xd8\x00\x06\x00\xdd\x00\xd3\x00\x05\x00\x87\x00\x88\x00\x06\x00\xdc\x00\x05\x00\xd1\x00\x07\x00\x06\x00\x87\x00\x88\x00\x9a\x00\x05\x00\x07\x00\xde\x00\x06\x00\xe7\x00\x05\x00\x07\x00\x62\x00\x06\x00\xe8\x00\x85\x00\xea\x00\x86\x00\x07\x00\x00\x01\xad\x00\x80\x00\x81\x00\x07\x00\xc3\x00\xb9\x00\x85\x00\xce\xff\x86\x00\xaf\x00\xbb\x00\x87\x00\x88\x00\x85\x00\xb1\x00\x86\x00\x85\x00\xb2\x00\x86\x00\xa9\x00\xaa\x00\xbc\x00\x87\x00\x88\x00\xc0\x00\xc3\x00\x07\x00\x3b\x00\x3c\x00\x87\x00\x88\x00\xc8\x00\x87\x00\x88\x00\x7f\x00\x80\x00\x81\x00\x82\x00\xc4\x00\x8a\x00\x80\x00\x81\x00\x8b\x00\xa7\x00\x81\x00\xdf\x00\xe0\x00\xc6\x00\x83\x00\x8c\x00\xe1\x00\xad\x00\x80\x00\x81\x00\x07\x00\x46\x00\xc9\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x07\x00\x46\x00\x8e\x00\x63\x00\x48\x00\x49\x00\x4a\x00\x07\x00\x46\x00\xcd\x00\x47\x00\x48\x00\x49\x00\x4a\x00\xce\x00\x9f\x00\xa0\x00\x80\x00\x81\x00\xa1\x00\xd3\x00\xad\x00\x80\x00\x81\x00\xd5\x00\x69\x00\x6a\x00\xd6\x00\xde\x00\xad\x00\x80\x00\x81\x00\xea\x00\xad\x00\x80\x00\x81\x00\xac\x00\xad\x00\x80\x00\x81\x00\xaf\x00\xad\x00\x80\x00\x81\x00\xbe\x00\x80\x00\x81\x00\xcb\x00\x80\x00\x81\x00\xb5\x00\xaa\x00\xb6\x00\xaa\x00\xb7\x00\xaa\x00\x90\x00\x1b\x00\x35\x00\x03\x00\x62\x00\x1b\x00\x35\x00\x03\x00\x43\x00\x44\x00\x35\x00\x03\x00\x22\x00\x1b\x00\xa6\x00\x29\x00\x2a\x00\xa7\x00\xd0\x00\x9f\x00\xac\x00\xb3\x00\xf5\xff\xb5\x00\x8f\x00\x7c\x00\x95\x00\x98\x00\x3b\x00\x9b\x00\x9c\x00\x99\x00\x7d\x00\x7e\x00\x7f\x00\x89\x00\x8a\x00\x90\x00\x66\x00\x92\x00\x65\x00\x67\x00\x7b\x00\x5b\x00\x5c\x00\x53\x00\x2e\x00\x32\x00\x33\x00\x34\x00\x3a\x00\x37\x00\x43\x00\x35\x00\x1a\x00\x2d\x00\x00\x00\x00\x00\x00\x00\x42\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (2, 134) [
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122),
	(123 , happyReduce_123),
	(124 , happyReduce_124),
	(125 , happyReduce_125),
	(126 , happyReduce_126),
	(127 , happyReduce_127),
	(128 , happyReduce_128),
	(129 , happyReduce_129),
	(130 , happyReduce_130),
	(131 , happyReduce_131),
	(132 , happyReduce_132),
	(133 , happyReduce_133),
	(134 , happyReduce_134)
	]

happy_n_terms = 43 :: Int
happy_n_nonterms = 61 :: Int

happyReduce_2 = happySpecReduce_1  0# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 (happy_var_1
	)}

happyReduce_3 = happySpecReduce_3  1# happyReduction_3
happyReduction_3 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { happy_var_2 -> 
	happyIn6
		 (tokenL happy_var_2
	)}

happyReduce_4 = happySpecReduce_1  1# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn6
		 (tokenL happy_var_1
	)}

happyReduce_5 = happySpecReduce_1  1# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOutTok happy_x_1 of { ((happy_var_1, T TRop ":")) -> 
	happyIn6
		 (Loc ":" (FilePos happy_var_1)
	)}

happyReduce_6 = happySpecReduce_3  2# happyReduction_6
happyReduction_6 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_2 of { happy_var_2 -> 
	happyIn7
		 (ListLit happy_var_2
	)}

happyReduce_7 = happySpecReduce_1  3# happyReduction_7
happyReduction_7 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (IntLit (tkVal happy_var_1)
	)}

happyReduce_8 = happySpecReduce_1  3# happyReduction_8
happyReduction_8 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (StrLit (tkVal happy_var_1)
	)}

happyReduce_9 = happySpecReduce_1  3# happyReduction_9
happyReduction_9 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (happy_var_1
	)}

happyReduce_10 = happySpecReduce_1  4# happyReduction_10
happyReduction_10 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (VarPattern (tkVal happy_var_1)
	)}

happyReduce_11 = happySpecReduce_1  4# happyReduction_11
happyReduction_11 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (CtrPattern (tkVal happy_var_1) []
	)}

happyReduce_12 = happySpecReduce_3  4# happyReduction_12
happyReduction_12 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_2 of { happy_var_2 -> 
	happyIn9
		 (makeCtrPattern happy_var_2
	)}

happyReduce_13 = happySpecReduce_1  4# happyReduction_13
happyReduction_13 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (LitPattern happy_var_1
	)}

happyReduce_14 = happySpecReduce_1  4# happyReduction_14
happyReduction_14 happy_x_1
	 =  happyIn9
		 (Wildcard
	)

happyReduce_15 = happySpecReduce_1  5# happyReduction_15
happyReduction_15 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (unA happy_var_1
	)}

happyReduce_16 = happySpecReduce_3  5# happyReduction_16
happyReduction_16 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn10
		 ([CtrPattern ":" [happy_var_1,happy_var_3]]
	)}}

happyReduce_17 = happySpecReduce_0  6# happyReduction_17
happyReduction_17  =  happyIn11
		 (id
	)

happyReduce_18 = happySpecReduce_1  6# happyReduction_18
happyReduction_18 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn11
		 (happy_var_1
	)}

happyReduce_19 = happySpecReduce_1  7# happyReduction_19
happyReduction_19 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn12
		 (consA happy_var_1
	)}

happyReduce_20 = happySpecReduce_3  7# happyReduction_20
happyReduction_20 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 (contA happy_var_1 happy_var_3
	)}}

happyReduce_21 = happySpecReduce_1  8# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn13
		 (VarPattern (tkVal happy_var_1)
	)}

happyReduce_22 = happySpecReduce_2  8# happyReduction_22
happyReduction_22 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut14 happy_x_2 of { happy_var_2 -> 
	happyIn13
		 (CtrPattern (tkVal happy_var_1) happy_var_2
	)}}

happyReduce_23 = happySpecReduce_3  8# happyReduction_23
happyReduction_23 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_2 of { happy_var_2 -> 
	happyIn13
		 (makeCtrPattern happy_var_2
	)}

happyReduce_24 = happySpecReduce_1  8# happyReduction_24
happyReduction_24 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 (LitPattern happy_var_1
	)}

happyReduce_25 = happySpecReduce_1  8# happyReduction_25
happyReduction_25 happy_x_1
	 =  happyIn13
		 (Wildcard
	)

happyReduce_26 = happySpecReduce_1  9# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	happyIn14
		 (unA happy_var_1
	)}

happyReduce_27 = happySpecReduce_2  10# happyReduction_27
happyReduction_27 happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_2 of { happy_var_2 -> 
	happyIn15
		 (contA happy_var_1 happy_var_2
	)}}

happyReduce_28 = happySpecReduce_0  10# happyReduction_28
happyReduction_28  =  happyIn15
		 (id
	)

happyReduce_29 = happySpecReduce_1  11# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 (unA happy_var_1
	)}

happyReduce_30 = happySpecReduce_1  12# happyReduction_30
happyReduction_30 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn17
		 (consA happy_var_1
	)}

happyReduce_31 = happySpecReduce_3  12# happyReduction_31
happyReduction_31 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn17
		 (contA happy_var_1 happy_var_3
	)}}

happyReduce_32 = happyMonadReduce 1# 13# happyReduction_32
happyReduction_32 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	( makeTyParam happy_var_1)}
	) (\r -> happyReturn (happyIn18 r))

happyReduce_33 = happySpecReduce_1  14# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 (unA happy_var_1
	)}

happyReduce_34 = happySpecReduce_0  15# happyReduction_34
happyReduction_34  =  happyIn20
		 (id
	)

happyReduce_35 = happySpecReduce_2  15# happyReduction_35
happyReduction_35 happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_2 of { happy_var_2 -> 
	happyIn20
		 (contA happy_var_1 happy_var_2
	)}}

happyReduce_36 = happySpecReduce_3  16# happyReduction_36
happyReduction_36 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn21
		 (tyArrow happy_var_1 happy_var_3
	)}}

happyReduce_37 = happySpecReduce_1  16# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 (happy_var_1
	)}

happyReduce_38 = happySpecReduce_2  17# happyReduction_38
happyReduction_38 happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_2 of { happy_var_2 -> 
	happyIn22
		 (STyApp happy_var_1 happy_var_2
	)}}

happyReduce_39 = happySpecReduce_1  17# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (happy_var_1
	)}

happyReduce_40 = happySpecReduce_1  18# happyReduction_40
happyReduction_40 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn23
		 (STyVar (tkVal happy_var_1)
	)}

happyReduce_41 = happyReduce 5# 18# happyReduction_41
happyReduction_41 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut21 happy_x_2 of { happy_var_2 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn23
		 (STyTuple (happy_var_2 : happy_var_4)
	) `HappyStk` happyRest}}

happyReduce_42 = happySpecReduce_3  18# happyReduction_42
happyReduction_42 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_2 of { happy_var_2 -> 
	happyIn23
		 (STyList happy_var_2
	)}

happyReduce_43 = happySpecReduce_2  18# happyReduction_43
happyReduction_43 happy_x_2
	happy_x_1
	 =  happyIn23
		 (STyCtr "[]"
	)

happyReduce_44 = happySpecReduce_3  18# happyReduction_44
happyReduction_44 happy_x_3
	happy_x_2
	happy_x_1
	 =  happyIn23
		 (STyCtr "(,)"
	)

happyReduce_45 = happySpecReduce_3  18# happyReduction_45
happyReduction_45 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_2 of { happy_var_2 -> 
	happyIn23
		 (happy_var_2
	)}

happyReduce_46 = happySpecReduce_1  18# happyReduction_46
happyReduction_46 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn23
		 (happy_var_1
	)}

happyReduce_47 = happySpecReduce_2  19# happyReduction_47
happyReduction_47 happy_x_2
	happy_x_1
	 =  happyIn24
		 (tyUnit
	)

happyReduce_48 = happySpecReduce_1  19# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn24
		 (STyCtr (tkVal happy_var_1)
	)}

happyReduce_49 = happyMonadReduce 1# 20# happyReduction_49
happyReduction_49 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut22 happy_x_1 of { happy_var_1 -> 
	( makeContext happy_var_1)}
	) (\r -> happyReturn (happyIn25 r))

happyReduce_50 = happySpecReduce_1  21# happyReduction_50
happyReduction_50 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (tokenL happy_var_1
	)}

happyReduce_51 = happySpecReduce_3  21# happyReduction_51
happyReduction_51 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { happy_var_2 -> 
	happyIn26
		 (tokenL happy_var_2
	)}

happyReduce_52 = happySpecReduce_1  22# happyReduction_52
happyReduction_52 happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn27
		 (listL (unA happy_var_1)
	)}

happyReduce_53 = happySpecReduce_1  23# happyReduction_53
happyReduction_53 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 (consA happy_var_1
	)}

happyReduce_54 = happySpecReduce_3  23# happyReduction_54
happyReduction_54 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (contA happy_var_1 happy_var_3
	)}}

happyReduce_55 = happyMonadReduce 6# 24# happyReduction_55
happyReduction_55 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut27 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	case happyOut21 happy_x_5 of { happy_var_5 -> 
	( makeDefSig happy_var_1 happy_var_3 happy_var_5)}}}
	) (\r -> happyReturn (happyIn29 r))

happyReduce_56 = happyMonadReduce 4# 24# happyReduction_56
happyReduction_56 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut27 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	( makeDefSig happy_var_1 [] happy_var_3)}}
	) (\r -> happyReturn (happyIn29 r))

happyReduce_57 = happySpecReduce_1  25# happyReduction_57
happyReduction_57 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn30
		 (unA happy_var_1
	)}

happyReduce_58 = happySpecReduce_0  26# happyReduction_58
happyReduction_58  =  happyIn31
		 (id
	)

happyReduce_59 = happySpecReduce_2  26# happyReduction_59
happyReduction_59 happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut29 happy_x_2 of { happy_var_2 -> 
	happyIn31
		 (contA happy_var_1 happy_var_2
	)}}

happyReduce_60 = happyMonadReduce 6# 27# happyReduction_60
happyReduction_60 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut25 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	case happyOut30 happy_x_5 of { happy_var_5 -> 
	( makeTypeClass happy_var_1 happy_var_3 happy_var_5)}}}
	) (\r -> happyReturn (happyIn32 r))

happyReduce_61 = happyMonadReduce 4# 27# happyReduction_61
happyReduction_61 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	( makeTypeClass [] happy_var_1 happy_var_3)}}
	) (\r -> happyReturn (happyIn32 r))

happyReduce_62 = happySpecReduce_1  28# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	happyIn33
		 (unA happy_var_1
	)}

happyReduce_63 = happySpecReduce_0  29# happyReduction_63
happyReduction_63  =  happyIn34
		 (id
	)

happyReduce_64 = happySpecReduce_2  29# happyReduction_64
happyReduction_64 happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_2 of { happy_var_2 -> 
	happyIn34
		 (contA happy_var_1 happy_var_2
	)}}

happyReduce_65 = happySpecReduce_1  30# happyReduction_65
happyReduction_65 happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	happyIn35
		 (InstTyDef happy_var_1
	)}

happyReduce_66 = happySpecReduce_1  30# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOut61 happy_x_1 of { happy_var_1 -> 
	happyIn35
		 (InstValDef (unL happy_var_1)
	)}

happyReduce_67 = happyMonadReduce 6# 31# happyReduction_67
happyReduction_67 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut25 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	case happyOut33 happy_x_5 of { happy_var_5 -> 
	( makeInstance happy_var_1 happy_var_3 happy_var_5)}}}
	) (\r -> happyReturn (happyIn36 r))

happyReduce_68 = happyMonadReduce 4# 31# happyReduction_68
happyReduction_68 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_3 of { happy_var_3 -> 
	( makeInstance [] happy_var_1 happy_var_3)}}
	) (\r -> happyReturn (happyIn36 r))

happyReduce_69 = happyMonadReduce 3# 32# happyReduction_69
happyReduction_69 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_3 of { happy_var_3 -> 
	( makeDataCtrL happy_var_1 [] happy_var_3)}}
	) (\r -> happyReturn (happyIn37 r))

happyReduce_70 = happyMonadReduce 1# 32# happyReduction_70
happyReduction_70 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	( makeDataCtrL happy_var_1 [] [])}
	) (\r -> happyReturn (happyIn37 r))

happyReduce_71 = happyMonadReduce 3# 33# happyReduction_71
happyReduction_71 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	( makeField happy_var_1 happy_var_3)}}
	) (\r -> happyReturn (happyIn38 r))

happyReduce_72 = happySpecReduce_1  34# happyReduction_72
happyReduction_72 happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn39
		 (unA happy_var_1
	)}

happyReduce_73 = happySpecReduce_0  35# happyReduction_73
happyReduction_73  =  happyIn40
		 (id
	)

happyReduce_74 = happySpecReduce_3  35# happyReduction_74
happyReduction_74 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	case happyOut38 happy_x_2 of { happy_var_2 -> 
	happyIn40
		 (contA happy_var_1 happy_var_2
	)}}

happyReduce_75 = happyMonadReduce 1# 36# happyReduction_75
happyReduction_75 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	( makeDataCtrL happy_var_1 [] [])}
	) (\r -> happyReturn (happyIn41 r))

happyReduce_76 = happyMonadReduce 5# 36# happyReduction_76
happyReduction_76 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_3 of { happy_var_3 -> 
	case happyOut41 happy_x_5 of { happy_var_5 -> 
	( makeDataCtrL happy_var_1 happy_var_3 happy_var_5)}}}
	) (\r -> happyReturn (happyIn41 r))

happyReduce_77 = happyMonadReduce 3# 36# happyReduction_77
happyReduction_77 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut41 happy_x_3 of { happy_var_3 -> 
	( makeDataCtrL happy_var_1 [] happy_var_3)}}
	) (\r -> happyReturn (happyIn41 r))

happyReduce_78 = happyMonadReduce 4# 36# happyReduction_78
happyReduction_78 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_3 of { happy_var_3 -> 
	( makeDataCtrL happy_var_1 happy_var_3 [])}}
	) (\r -> happyReturn (happyIn41 r))

happyReduce_79 = happyMonadReduce 5# 37# happyReduction_79
happyReduction_79 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { ((happy_var_1, T TRes "enum")) -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut37 happy_x_4 of { happy_var_4 -> 
	( makeEnum happy_var_1 happy_var_2 happy_var_4)}}}
	) (\r -> happyReturn (happyIn42 r))

happyReduce_80 = happyMonadReduce 6# 37# happyReduction_80
happyReduction_80 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { ((happy_var_1, T TRes "data")) -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut19 happy_x_3 of { happy_var_3 -> 
	case happyOut39 happy_x_5 of { happy_var_5 -> 
	( makeDataS happy_var_1 happy_var_2 happy_var_3 happy_var_5)}}}}
	) (\r -> happyReturn (happyIn42 r))

happyReduce_81 = happyMonadReduce 6# 37# happyReduction_81
happyReduction_81 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { ((happy_var_1, T TRes "data")) -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut19 happy_x_3 of { happy_var_3 -> 
	case happyOut41 happy_x_5 of { happy_var_5 -> 
	( makeData happy_var_1 happy_var_2 happy_var_3 happy_var_5)}}}}
	) (\r -> happyReturn (happyIn42 r))

happyReduce_82 = happyMonadReduce 8# 37# happyReduction_82
happyReduction_82 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { ((happy_var_1, T TRes "newtype")) -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut19 happy_x_3 of { happy_var_3 -> 
	case happyOutTok happy_x_5 of { happy_var_5 -> 
	case happyOut38 happy_x_7 of { happy_var_7 -> 
	( makeDataN happy_var_1 happy_var_2 happy_var_3 happy_var_5 happy_var_7)}}}}}
	) (\r -> happyReturn (happyIn42 r))

happyReduce_83 = happySpecReduce_0  38# happyReduction_83
happyReduction_83  =  happyIn43
		 (Nothing
	)

happyReduce_84 = happySpecReduce_2  38# happyReduction_84
happyReduction_84 happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_2 of { happy_var_2 -> 
	happyIn43
		 (Just happy_var_2
	)}

happyReduce_85 = happyMonadReduce 7# 39# happyReduction_85
happyReduction_85 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { ((happy_var_1, T TRes "state")) -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut19 happy_x_3 of { happy_var_3 -> 
	case happyOut43 happy_x_4 of { happy_var_4 -> 
	case happyOut39 happy_x_6 of { happy_var_6 -> 
	( makeState happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_6)}}}}}
	) (\r -> happyReturn (happyIn44 r))

happyReduce_86 = happySpecReduce_3  40# happyReduction_86
happyReduction_86 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOut54 happy_x_3 of { happy_var_3 -> 
	happyIn45
		 (Alt happy_var_1 happy_var_3
	)}}

happyReduce_87 = happySpecReduce_1  41# happyReduction_87
happyReduction_87 happy_x_1
	 =  case happyOut47 happy_x_1 of { happy_var_1 -> 
	happyIn46
		 (unA happy_var_1
	)}

happyReduce_88 = happySpecReduce_1  42# happyReduction_88
happyReduction_88 happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (consA happy_var_1
	)}

happyReduce_89 = happySpecReduce_3  42# happyReduction_89
happyReduction_89 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn47
		 (contA happy_var_1 happy_var_3
	)}}

happyReduce_90 = happyReduce 4# 43# happyReduction_90
happyReduction_90 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut9 happy_x_1 of { happy_var_1 -> 
	case happyOut54 happy_x_3 of { happy_var_3 -> 
	happyIn48
		 (Option happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_91 = happySpecReduce_1  44# happyReduction_91
happyReduction_91 happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	happyIn49
		 (unA happy_var_1
	)}

happyReduce_92 = happySpecReduce_1  45# happyReduction_92
happyReduction_92 happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	happyIn50
		 (consA happy_var_1
	)}

happyReduce_93 = happySpecReduce_2  45# happyReduction_93
happyReduction_93 happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	case happyOut48 happy_x_2 of { happy_var_2 -> 
	happyIn50
		 (contA happy_var_1 happy_var_2
	)}}

happyReduce_94 = happySpecReduce_1  46# happyReduction_94
happyReduction_94 happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	happyIn51
		 (unA happy_var_1
	)}

happyReduce_95 = happySpecReduce_0  47# happyReduction_95
happyReduction_95  =  happyIn52
		 (id
	)

happyReduce_96 = happySpecReduce_2  47# happyReduction_96
happyReduction_96 happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_2 of { happy_var_2 -> 
	happyIn52
		 (contA happy_var_1 happy_var_2
	)}}

happyReduce_97 = happySpecReduce_2  48# happyReduction_97
happyReduction_97 happy_x_2
	happy_x_1
	 =  case happyOut54 happy_x_1 of { happy_var_1 -> 
	happyIn53
		 (Statement happy_var_1
	)}

happyReduce_98 = happyMonadReduce 4# 48# happyReduction_98
happyReduction_98 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut56 happy_x_1 of { happy_var_1 -> 
	case happyOut54 happy_x_3 of { happy_var_3 -> 
	( makeBind happy_var_1 happy_var_3)}}
	) (\r -> happyReturn (happyIn53 r))

happyReduce_99 = happyMonadReduce 4# 48# happyReduction_99
happyReduction_99 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut56 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	( makeGetter happy_var_1 happy_var_3)}}
	) (\r -> happyReturn (happyIn53 r))

happyReduce_100 = happyMonadReduce 4# 48# happyReduction_100
happyReduction_100 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut54 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	( makeSetter happy_var_1 happy_var_3)}}
	) (\r -> happyReturn (happyIn53 r))

happyReduce_101 = happySpecReduce_1  49# happyReduction_101
happyReduction_101 happy_x_1
	 =  case happyOut55 happy_x_1 of { happy_var_1 -> 
	happyIn54
		 (happy_var_1
	)}

happyReduce_102 = happySpecReduce_3  49# happyReduction_102
happyReduction_102 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut54 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_2 of { happy_var_2 -> 
	case happyOut55 happy_x_3 of { happy_var_3 -> 
	happyIn54
		 (InfixOp (unL happy_var_2) happy_var_1 happy_var_3
	)}}}

happyReduce_103 = happyReduce 4# 50# happyReduction_103
happyReduction_103 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut14 happy_x_2 of { happy_var_2 -> 
	case happyOut54 happy_x_4 of { happy_var_4 -> 
	happyIn55
		 (Abs (Alt happy_var_2 happy_var_4)
	) `HappyStk` happyRest}}

happyReduce_104 = happyReduce 6# 50# happyReduction_104
happyReduction_104 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut54 happy_x_2 of { happy_var_2 -> 
	case happyOut49 happy_x_5 of { happy_var_5 -> 
	happyIn55
		 (Case happy_var_2 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_105 = happyMonadReduce 6# 50# happyReduction_105
happyReduction_105 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut54 happy_x_2 of { happy_var_2 -> 
	case happyOut54 happy_x_4 of { happy_var_4 -> 
	case happyOut54 happy_x_6 of { happy_var_6 -> 
	( makeCond happy_var_2 happy_var_4 happy_var_6)}}}
	) (\r -> happyReturn (happyIn55 r))

happyReduce_106 = happyMonadReduce 4# 50# happyReduction_106
happyReduction_106 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { ((happy_var_1, T TRes "let")) -> 
	case happyOut46 happy_x_2 of { happy_var_2 -> 
	case happyOut54 happy_x_4 of { happy_var_4 -> 
	( makeLet happy_var_1 happy_var_2 happy_var_4)}}}
	) (\r -> happyReturn (happyIn55 r))

happyReduce_107 = happySpecReduce_1  50# happyReduction_107
happyReduction_107 happy_x_1
	 =  case happyOut56 happy_x_1 of { happy_var_1 -> 
	happyIn55
		 (happy_var_1
	)}

happyReduce_108 = happySpecReduce_2  51# happyReduction_108
happyReduction_108 happy_x_2
	happy_x_1
	 =  case happyOut56 happy_x_1 of { happy_var_1 -> 
	case happyOut57 happy_x_2 of { happy_var_2 -> 
	happyIn56
		 (App happy_var_1 happy_var_2
	)}}

happyReduce_109 = happySpecReduce_1  51# happyReduction_109
happyReduction_109 happy_x_1
	 =  case happyOut57 happy_x_1 of { happy_var_1 -> 
	happyIn56
		 (happy_var_1
	)}

happyReduce_110 = happySpecReduce_1  52# happyReduction_110
happyReduction_110 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn57
		 (Var (tkVal happy_var_1)
	)}

happyReduce_111 = happySpecReduce_1  52# happyReduction_111
happyReduction_111 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn57
		 (Ctr (tkVal happy_var_1)
	)}

happyReduce_112 = happyMonadReduce 3# 52# happyReduction_112
happyReduction_112 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut51 happy_x_2 of { happy_var_2 -> 
	( checkStmts happy_var_2)}
	) (\r -> happyReturn (happyIn57 r))

happyReduce_113 = happySpecReduce_1  52# happyReduction_113
happyReduction_113 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn57
		 (Lit happy_var_1
	)}

happyReduce_114 = happySpecReduce_3  52# happyReduction_114
happyReduction_114 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_2 of { happy_var_2 -> 
	happyIn57
		 (makeParensExpr happy_var_2
	)}

happyReduce_115 = happySpecReduce_1  53# happyReduction_115
happyReduction_115 happy_x_1
	 =  case happyOut59 happy_x_1 of { happy_var_1 -> 
	happyIn58
		 (unA happy_var_1
	)}

happyReduce_116 = happySpecReduce_0  54# happyReduction_116
happyReduction_116  =  happyIn59
		 (id
	)

happyReduce_117 = happySpecReduce_1  54# happyReduction_117
happyReduction_117 happy_x_1
	 =  case happyOut60 happy_x_1 of { happy_var_1 -> 
	happyIn59
		 (happy_var_1
	)}

happyReduce_118 = happySpecReduce_1  54# happyReduction_118
happyReduction_118 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn59
		 (consA (Var (unL happy_var_1))
	)}

happyReduce_119 = happySpecReduce_1  55# happyReduction_119
happyReduction_119 happy_x_1
	 =  case happyOut54 happy_x_1 of { happy_var_1 -> 
	happyIn60
		 (consA happy_var_1
	)}

happyReduce_120 = happySpecReduce_3  55# happyReduction_120
happyReduction_120 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut60 happy_x_1 of { happy_var_1 -> 
	case happyOut54 happy_x_3 of { happy_var_3 -> 
	happyIn60
		 (contA happy_var_1 happy_var_3
	)}}

happyReduce_121 = happyMonadReduce 5# 56# happyReduction_121
happyReduction_121 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut14 happy_x_2 of { happy_var_2 -> 
	case happyOut54 happy_x_4 of { happy_var_4 -> 
	( makeEquation happy_var_1 happy_var_2 happy_var_4)}}}
	) (\r -> happyReturn (happyIn61 r))

happyReduce_122 = happyMonadReduce 6# 56# happyReduction_122
happyReduction_122 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut9 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut14 happy_x_3 of { happy_var_3 -> 
	case happyOut54 happy_x_5 of { happy_var_5 -> 
	( makeEquation (tokenL happy_var_2) (happy_var_1:happy_var_3) happy_var_5)}}}}
	) (\r -> happyReturn (happyIn61 r))

happyReduce_123 = happyMonadReduce 5# 57# happyReduction_123
happyReduction_123 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { ((happy_var_1, T TRes "module")) -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut63 happy_x_4 of { happy_var_4 -> 
	( makeModule happy_var_1 happy_var_2 happy_var_4)}}}
	) (\r -> happyReturn (happyIn62 r))

happyReduce_124 = happySpecReduce_1  58# happyReduction_124
happyReduction_124 happy_x_1
	 =  case happyOut64 happy_x_1 of { happy_var_1 -> 
	happyIn63
		 (unA happy_var_1
	)}

happyReduce_125 = happySpecReduce_0  59# happyReduction_125
happyReduction_125  =  happyIn64
		 (id
	)

happyReduce_126 = happySpecReduce_2  59# happyReduction_126
happyReduction_126 happy_x_2
	happy_x_1
	 =  case happyOut64 happy_x_1 of { happy_var_1 -> 
	case happyOut65 happy_x_2 of { happy_var_2 -> 
	happyIn64
		 (contA happy_var_1 happy_var_2
	)}}

happyReduce_127 = happyMonadReduce 3# 60# happyReduction_127
happyReduction_127 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { ((happy_var_1, T TRes "import")) -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	( makeImportDef happy_var_1 happy_var_2)}}
	) (\r -> happyReturn (happyIn65 r))

happyReduce_128 = happyMonadReduce 6# 60# happyReduction_128
happyReduction_128 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { ((happy_var_1, T TRes "type")) -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut19 happy_x_3 of { happy_var_3 -> 
	case happyOut21 happy_x_5 of { happy_var_5 -> 
	( makeTypeDef happy_var_1 happy_var_2 happy_var_3 happy_var_5)}}}}
	) (\r -> happyReturn (happyIn65 r))

happyReduce_129 = happyMonadReduce 2# 60# happyReduction_129
happyReduction_129 (happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { ((happy_var_1, T TRes "class")) -> 
	case happyOut32 happy_x_2 of { happy_var_2 -> 
	( makeTyClDef happy_var_1 happy_var_2)}}
	) (\r -> happyReturn (happyIn65 r))

happyReduce_130 = happyMonadReduce 2# 60# happyReduction_130
happyReduction_130 (happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { ((happy_var_1, T TRes "instance")) -> 
	case happyOut36 happy_x_2 of { happy_var_2 -> 
	( makeInstDef happy_var_1 happy_var_2)}}
	) (\r -> happyReturn (happyIn65 r))

happyReduce_131 = happySpecReduce_1  60# happyReduction_131
happyReduction_131 happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn65
		 (happy_var_1
	)}

happyReduce_132 = happySpecReduce_1  60# happyReduction_132
happyReduction_132 happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	happyIn65
		 (happy_var_1
	)}

happyReduce_133 = happySpecReduce_1  60# happyReduction_133
happyReduction_133 happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	happyIn65
		 (makeTypeDec happy_var_1
	)}

happyReduce_134 = happyMonadReduce 1# 60# happyReduction_134
happyReduction_134 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut61 happy_x_1 of { happy_var_1 -> 
	( makeValDef happy_var_1)}
	) (\r -> happyReturn (happyIn65 r))

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	(_, T TEoF "") -> happyDoAction 42# tk action sts stk;
	(happy_dollar_dollar, T TSpecial "{") -> cont 1#;
	(happy_dollar_dollar, T TSpecial "}") -> cont 2#;
	(happy_dollar_dollar, T TSpecial ";") -> cont 3#;
	(happy_dollar_dollar, T TSpecial "(") -> cont 4#;
	(happy_dollar_dollar, T TSpecial ")") -> cont 5#;
	(happy_dollar_dollar, T TSpecial "[") -> cont 6#;
	(happy_dollar_dollar, T TSpecial "]") -> cont 7#;
	(happy_dollar_dollar, T TSpecial ",") -> cont 8#;
	(happy_dollar_dollar, T TSpecial "`") -> cont 9#;
	(happy_dollar_dollar, T TSpecial "!") -> cont 10#;
	(happy_dollar_dollar, T TRop "=") -> cont 11#;
	(happy_dollar_dollar, T TRop ":") -> cont 12#;
	(happy_dollar_dollar, T TRop "->") -> cont 13#;
	(happy_dollar_dollar, T TRop "<-") -> cont 14#;
	(happy_dollar_dollar, T TRop "::") -> cont 15#;
	(happy_dollar_dollar, T TRop "=>") -> cont 16#;
	(happy_dollar_dollar, T TRop "\\") -> cont 17#;
	(happy_dollar_dollar, T TRop "<:") -> cont 18#;
	(happy_dollar_dollar, T TRop ">:") -> cont 19#;
	(_, T TVar _) -> cont 20#;
	(_, T TCtr _) -> cont 21#;
	(_, T TVarSym _) -> cont 22#;
	(_, T TInt _) -> cont 23#;
	(_, T TStr _) -> cont 24#;
	(happy_dollar_dollar, T TRes "case") -> cont 25#;
	(happy_dollar_dollar, T TRes "class") -> cont 26#;
	(happy_dollar_dollar, T TRes "data") -> cont 27#;
	(happy_dollar_dollar, T TRes "else") -> cont 28#;
	(happy_dollar_dollar, T TRes "enum") -> cont 29#;
	(happy_dollar_dollar, T TRes "if") -> cont 30#;
	(happy_dollar_dollar, T TRes "in") -> cont 31#;
	(happy_dollar_dollar, T TRes "instance") -> cont 32#;
	(happy_dollar_dollar, T TRes "import") -> cont 33#;
	(happy_dollar_dollar, T TRes "let") -> cont 34#;
	(happy_dollar_dollar, T TRes "module") -> cont 35#;
	(happy_dollar_dollar, T TRes "newtype") -> cont 36#;
	(happy_dollar_dollar, T TRes "of") -> cont 37#;
	(happy_dollar_dollar, T TRes "state") -> cont 38#;
	(happy_dollar_dollar, T TRes "then") -> cont 39#;
	(happy_dollar_dollar, T TRes "type") -> cont 40#;
	(happy_dollar_dollar, T TRes "_") -> cont 41#;
	_ -> happyError' tk
	})

happyError_ 42# tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => Parser a -> (a -> Parser b) -> Parser b
happyThen = (>>=)
happyReturn :: () => a -> Parser a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> Parser a
happyReturn1 = happyReturn
happyError' :: () => (TokenP) -> Parser a
happyError' tk = parseError tk

parseModule = happySomeParser where
  happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (happyOut62 x))

parseExpr = happySomeParser where
  happySomeParser = happyThen (happyParse 1#) (\x -> happyReturn (happyOut55 x))

happySeq = happyDontSeq


{--------------------------------------------------------------------------}
{-- Auxiliary Parser Functions                                            -}
{--------------------------------------------------------------------------}
    
debug :: String -> a -> Parser a
debug m x = do
    p <- getPosP
    addInfoP (FilePos p) m
    return x
    
lexer :: (TokenP -> Parser a) -> Parser a
lexer f = P (\s -> case runAlexFrom alexMonadScan s of
    (Left err)      -> lexicalError err s >> failMT
    (Right (s', t)) -> let (P m) = f t in {-addInfo s' (show t) >>-} m s')

runCadaParser :: String -> (Maybe (LocP Module), ErrorS)
runCadaParser xs = runStateM (initParser parseModule xs) id

runExprParser :: String -> (Maybe Expr, ErrorS)
runExprParser xs = runStateM (initParser parseExpr xs) id

parseError :: TokenP -> Parser a
parseError (p,t) = failP p (ppParserError t)

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<inbyggd>" #-}
{-# LINE 1 "<kommandorad>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates\\GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





{-# LINE 51 "templates\\GenericTemplate.hs" #-}

{-# LINE 61 "templates\\GenericTemplate.hs" #-}

{-# LINE 70 "templates\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= {- nothing -}


	  case action of
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | (n Happy_GHC_Exts.<# (0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

				     (happyReduceArr Happy_Data_Array.! rule) i tk st
				     where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
		n		  -> {- nothing -}


				     happyShift new_state i tk st
				     where (new_state) = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where (off)    = indexShortOffAddr happyActOffsets st
         (off_i)  = (off Happy_GHC_Exts.+# i)
	 check  = if (off_i Happy_GHC_Exts.>=# (0# :: Happy_GHC_Exts.Int#))
			then (indexShortOffAddr happyCheck off_i Happy_GHC_Exts.==#  i)
			else False
         (action)
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st

{-# LINE 130 "templates\\GenericTemplate.hs" #-}


indexShortOffAddr (HappyA# arr) off =
	Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 163 "templates\\GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let (i) = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@((HappyCons (st1@(action)) (_)))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@((HappyCons (st1@(action)) (_)))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

             (off) = indexShortOffAddr happyGotoOffsets st1
             (off_i) = (off Happy_GHC_Exts.+# nt)
             (new_state) = indexShortOffAddr happyTable off_i




happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where (off) = indexShortOffAddr happyGotoOffsets st
         (off_i) = (off Happy_GHC_Exts.+# nt)
         (new_state) = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 0# tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
