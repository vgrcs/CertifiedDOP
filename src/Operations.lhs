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
-- Module      :  Operations
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  Vitor Rodrigues
-- Stability   :
-- Portability :
--
--
-----------------------------------------------------------------------------

module Operations ( runApply2, runCertify2 ) where

import Data.Map
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative hiding (empty)
import Functions
import Checker
import System.IO.Unsafe
import System.Exit

type Var         =  String
data AbstExpr    =  ParseDeltaRPM Var
                    | ApplyDeltaRPM Var
                    | MakeDeltaRPM Var
                    | GetSymbolsTable Var
                    | CheckProof Var
                    | InstallRPM Var
                    deriving (Eq, Show, Ord)

data ConcExpr    =  F String (Internal -> Internal)

data BoolExpr    =  Check AbstExpr

data Command     =  Assign Var AbstExpr
                   | Seq Command  Command
                   | Cond BoolExpr Command Command
                   | Abort

type Env         =  Map String Internal
type MetaMap     =  Map AbstExpr ConcExpr

type Checker a   =  ReaderT MetaMap (StateT Env (IO)) a

runCheck                 ::  Checker a -> MetaMap -> Env ->  IO (a, Env)
runCheck eval meta env   =   runStateT (runReaderT eval meta) env

evalVar                  ::  Var -> Checker Internal
evalVar v                =   flip (!) v <$> get
                             >>= \val -> return val

evalExp                  ::  AbstExpr -> Checker Internal
evalExp e                =  flip (!) e <$> ask
                            >>= \(F var fun)  -> evalVar var
                            >>= \arg      -> return (fun arg)

evalBool                 ::  BoolExpr -> Checker Bool
evalBool (Check e)       =   evalExp e
                             >>= \v -> if  v == ProofChecker (ExitSuccess)
                                           then  return True
                                           else  return False

evalComm                 :: Command -> Checker ()
evalComm (Assign v e)    =  evalExp e
                            >>= \val -> put =<< insert v val <$> get

evalComm (Seq a b)       =  evalComm a >> evalComm b

evalComm (Cond b c1 c2)  =  evalBool b
                            >>= \checked -> if  checked
                                                then evalComm c1
                                                else evalComm c2

evalComm (Abort)         =  liftIO $ putStrLn ("Abort") >> exitFailure

metaFunctions  ::  [ (AbstExpr, ConcExpr) ]
metaFunctions  =   [ (ParseDeltaRPM "a" , F "a" readDeltaRPM),
                     (ApplyDeltaRPM "x", F "x" applyDeltaRPM),
                     (MakeDeltaRPM "m", F "m" makeDeltaRPM),
                     (GetSymbolsTable "y", F "y" getSymbolsTable),
                     (CheckProof "z", F "z" runChecker),
                     (InstallRPM "x", F "x" installRPM) ]



runCertify2
  :: FilePath ->
     FilePath ->
     FilePath ->
     FilePath ->
     IO ()
runCertify2 oldrpmdir newrpmdir workdir deltarpm
   = do
     let args = unsafePerformIO $ getCommandLine1 oldrpmdir newrpmdir workdir deltarpm
     (val, env) <- runCertifyAux args
     putStrLn (show env)

runCertifyAux
  ::  Internal -> IO ((), Env)
runCertifyAux args
  =  do
     let prog =  Seq (Assign "y" (MakeDeltaRPM "m")) ( Assign "z" (GetSymbolsTable "y"))
     runCheck (evalComm prog) (fromList metaFunctions) (insert "m" args empty)

runApply2
  :: FilePath ->
     FilePath ->
     IO (Int)
runApply2 workdir deltarpm
  = do

    let args = unsafePerformIO $  getCommandLine2 workdir deltarpm

    let a = Assign "x" (ParseDeltaRPM "a")
        b = Assign "y" (ApplyDeltaRPM "x")
        c = Assign "z" (GetSymbolsTable "y")
        i = Assign "i" (InstallRPM "x")

        cond = Cond (Check (CheckProof "z")) i Abort
        prog = Seq a (Seq b (Seq c cond))

    (val, env) <- runCheck (evalComm prog) (fromList metaFunctions) (insert "a" args empty)

    let (ValueList (_:reconstructed:xs)) = env ! "x"
        result = env ! "i"
    let b = if result == ProofChecker ExitSuccess then 0 else 2
    return b

runApplyAux
  ::  Internal -> IO ((), Env)
runApplyAux args
  =  do
     let  a = Assign "x" (ParseDeltaRPM "a")
          b = Assign "y" (ApplyDeltaRPM "x")
          c = Assign "z" (GetSymbolsTable "y")
          i = Assign "i" (InstallRPM "x")
          prog = Seq a (Seq b (Seq c (Cond (Check (CheckProof "z")) i Abort)))

     runCheck  (evalComm prog) (fromList metaFunctions)
               (insert "a" args empty)

\end{code}
\end{document}


