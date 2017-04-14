-----------------------------------------------------------------------------
--
-- Module      :  Extraction
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  Vitor Rodrigues
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
module Extraction (
    Atom(..), CoqValue(..), Expr(..), GoalType (..),
    Apply (..), TypedExpression (..), SafeTable (..), DialectType (..),
    BaseType (..), assertionH1
) where

--import Prelude
import Text.PrettyPrint
import Generics.Deriving.Base (Generic)
import Generics.Deriving.Show (GShow, gshow)
import InternalData

type Undefined_symbol = ()

data CoqValue =
   ListOperations ([] Prelude.String)
 | ListFilenames ([] Atom)
 | ListSymbols ([] Atom)
 | Product CoqValue CoqValue
 | Assemble ([] Atom)
    deriving (Generic)

assertionH1 :: [Atom] -> Doc
assertionH1 syms
    = let abst  = map (\(Symbol s _) -> Flag s) syms -- (take 2 syms)
          str = (foldl1 (<^>) (map (text .show) abst) <^> (text "nil"))
      in parens $
         text ("Resolved") <+> parens str

instance GShow CoqValue
instance Show CoqValue where
  show (ListSymbols []) = render $ (text "ListSymbols") <+> text "nil"
  show (ListSymbols ss) = render $ (text "ListSymbols") <+>
                            parens (foldl1 (<^>) (map (text .show) ss) <^> (text "nil"))
  show (Assemble [])    = render $ (text "List") <+> text "nil"
  show (Assemble ss)    = render $ (text "List") <+>
                            parens (foldl1 (<#>)
                                        (map (\s -> parens (text (show s) <^> (text "nil"))) ss)
                                        <+> (text "++ nil"))
  show (ListOperations []) = render $ text "nil"
  show (ListOperations ss) = render $ foldl1 (<^>)
                                      (map (\s -> text (show s ) -- <> text "%string"
                                                                 ) ss) <^>
                                      (text "nil")
  show other            = gshow other


data Expr =
   Statechart_elem Atom
 | Metadata_elem Atom
 | Object_elem Atom
 | Add_operation Expr Expr
 | Rem_operation Expr Expr
 | Delta [Expr]
 | Compile Expr
    deriving (Generic)

instance GShow Expr
instance Show Expr where
    show (Compile e)            =  render $ (text "Compile") <+> parens (text (show e))
    show (Delta [])             =  render $ (text "Delta") <+> (text "nil")
    show (Delta d)              =  render $ (text "Delta") <+>
                                    parens (foldl1 (<^>) (map (text .show) d) <^> (text "nil"))
    show (Add_operation e expr) = render $ parens ((text "Add_operation") <+>
                                                     parens (text (show e))
                                                      <+> parens (text (show expr)))
    show (Object_elem o)        = render $ (text (show o))
    show expr                   = render $ parens $ text $ gshow expr

tail_sym x = parens $ x <> text "++" <> text "nil"

x <^> y = x <> text "::" <> y
x <#> y = x <> text "++" <> y

data DialectType =
   Type_Base BaseType
 | Type_Delta BaseType [Atom]
 | Type_Product BaseType
 | Type_Variant BaseType
     deriving (Generic)

data GoalType = Goal DialectType deriving (Generic)


instance GShow DialectType
instance GShow BaseType
instance GShow GoalType

instance Show DialectType where
    show (Type_Delta Type_Object []) = "Type_Delta nil"
    show (Type_Delta Type_Object ss) =
        render $ (text "Type_Delta") <+> parens (foldl1 (<#>)
                        (map (\s -> parens (text (show s) <^> (text "nil"))) ss)
                            <+> (text "++ nil"))
    show other = gshow other

instance Show GoalType where
    show (Goal (Type_Delta Type_Object [])) = "Type_Delta nil"
    show (Goal (Type_Delta Type_Object ss)) =
        render $ (text "Type_Delta") <+> parens (foldl1 (<^>) (map (text .show) ss) <^> (text "nil"))
    show other = gshow other


data BaseType =
   Type_Statechart
 | Type_Metadata
 | Type_Object
     deriving (Show, Generic)

data Apply = Apply (Expr, CoqValue) deriving (Generic)

instance Show Apply where
    show (Apply (e, v)) = render $
                          (text "Apply") <+>  parens (text (show e) <> comma <+> text (show v))

data TypedExpression = TypedExpression (Expr, DialectType) deriving (Show, Generic)
data SafeTable = SafeTable (CoqValue, GoalType) deriving (Show, Generic)

