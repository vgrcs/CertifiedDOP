\documentclass{article}

%%include lhs2TeX.fmt
%%include lhs2TeX.sty
%%include spacing.fmt
%include polycode.fmt

%format <$> = "<\!\!\$\!\!>"
%format *> = "*\!\!>"
%format _ = "\anonymous\ "

\begin{document}
\begin{code}
-----------------------------------------------------------------------------
--
-- Module      :  Internal
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  Vitor Rodrigues
-- Stability   :
-- Portability :
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
module InternalData ( Atom (..), Internal (..)) where

import System.Exit
import System.Directory
import Path
import Text.PrettyPrint
import Generics.Deriving.Base (Generic)
import Generics.Deriving.Show (GShow, gshow)

data Atom      =  Symbol Char String | Flag Char
                  deriving (Generic, Eq)

data Internal  =  Table [Atom]
               |  RelDir (Path Rel Dir)
               |  RelFile (Path Rel File)
               |  ProofChecker ExitCode
               |  ValueList [Internal]
               |  Install Bool
                  deriving (Eq)

instance GShow Atom
instance Show Atom where
    show (Symbol s str) = render $ (text "Sym") <+>  (doubleQuotes (text [s])) <+>
                                                     (doubleQuotes (text str))
    show (Flag s      ) = render $
                            (doubleQuotes (text [s]))

instance Show Internal where
    show (RelFile f)      = fromRelFile f
    show (RelDir f)       = fromRelDir f
    show (ValueList t)    = show t
    show (Table v)        = show v
    show (ProofChecker v) = show v
    show (Install i)      = show i

\end{code}
\end{document}

