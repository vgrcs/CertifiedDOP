-----------------------------------------------------------------------------
--
-- Module      :  Checker
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
module Checker ( runChecker ) where

import Text.PrettyPrint

import Extraction
import Functions
import Generics.Deriving.Base (Generic)
import Generics.Deriving.Show (GShow, gshow)
import System.Process
import System.Exit
import System.Console.CmdArgs.Verbosity
import Control.Applicative hiding (empty)
import System.IO.Unsafe

infixr 1 :=>:

data COQFormula = CoqPTy SafeTable
                | CoqPEx TypedExpression
                | CoqPEv Apply
                | COQFormula :=>: COQFormula
                deriving (Generic)

instance GShow Apply
instance GShow TypedExpression
instance GShow SafeTable

instance GShow COQFormula
instance Show COQFormula where
    show (CoqPEv p)  = show p
    show (CoqPEx p)  = show p
    show (CoqPTy p)  = show p
    show (a :=>: b)  = show a ++ " -> " ++ "\n" ++ show b

encodeCOQFormula :: [Atom] -> COQFormula
encodeCOQFormula syms
    = -- (encodePEv syms) :=>:
      -- (encodePEx syms) :=>:
      (encodePTy syms)

encodePEx :: [Atom] -> COQFormula
encodePEx syms
    = let core =  ListSymbols []
          delta = Delta (map (\s -> Add_operation (Object_elem s) (Delta [])) syms) --(take 2 syms))
          abst = map (\(Symbol s _) -> Flag s) syms -- (take 2 syms)
      in (CoqPEx (TypedExpression (delta, Type_Delta Type_Object abst)))

encodePEv :: [Atom] -> COQFormula
encodePEv syms
    = let delta = Delta (map (\s -> Add_operation (Object_elem s) (Delta [])) syms) -- (take 2 syms) )
          value = Assemble syms -- (take 2 syms)
      in (CoqPEv (Apply (delta, value)))

encodePTy :: [Atom] -> COQFormula
encodePTy syms
    = let core =  ListSymbols []
          value = Assemble  syms -- (take 2 syms)
          abst = map (\(Symbol s _) -> Flag s)  syms -- (take 2 syms)
      in (CoqPTy (SafeTable (value, Goal (Type_Delta Type_Object abst))))




header = text "Add LoadPath \"usr/local/lib/pcc\" ." $+$
         text "Require Import CoqDeltaRPM." $+$
         text "Require Import List." $+$
         text "Require Import Ascii." $+$
         text "Require Import String."

lemma l = text "Lemma delta :" $+$
          text (show l) Text.PrettyPrint.<>
          text "."

proof syms
        = text "Proof." $+$
          --nest 2 (text "intros H1 H2.") $+$

          nest 2 ((text "assert") <+> parens (text (show (encodePEv syms)))) $+$
          nest 4 (text "by (solve_apply).") $+$

          nest 2 ((text "assert") <+> parens (text (show (encodePEx syms)))) $+$
          nest 4 (text "by (solve_typed_expr).") $+$

          nest 2 ((text "assert") <+> (assertionH1 syms)) $+$
          nest 4 (text "by (solve_consistency).") $+$
          nest 2 (text "eauto using TypeLemma.") $+$
          text "Qed."

runChecker :: Internal -> Internal
runChecker (Table syms)
 = ProofChecker <$> unsafePerformIO $ runCOQ (Table syms)

runCOQ :: Internal -> IO ExitCode
runCOQ (Table syms)
  = do
    let coq = encodeCOQFormula syms
        source = render $
                 header $+$
                 lemma coq $+$
                 proof syms

    writeFile ("vc.v") source
    whenLoud $ putStrLn =<< readFile "vc.v"
    whenLoud $ putStrLn $ (show (length syms))
    whenLoud $ putStrLn $ ("T => " ++ show (length (filter (\(Symbol t _) -> t == 'T') syms)))
    whenLoud $ putStrLn $ ("D => " ++ show (length (filter (\(Symbol t _) -> t == 'D') syms)))
    whenLoud $ putStrLn $ ("d => " ++ show (length (filter (\(Symbol t _) -> t == 'd') syms)))
    whenLoud $ putStrLn $ ("B => " ++ show (length (filter (\(Symbol t _) -> t == 'B') syms)))
    whenLoud $ putStrLn $ ("? => " ++ show (length (filter (\(Symbol t _) -> t == '?') syms)))

    let cmd = "coqc --verbose vc.v"
    (success, stdout, stderr) <- readCreateProcessWithExitCode (shell cmd) ""
    whenLoud $ putStrLn $ (show (cmd) ++ "\n" ++ show success ++ "\n" ++ show stderr)
    return success

