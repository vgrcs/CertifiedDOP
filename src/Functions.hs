-----------------------------------------------------------------------------
--
-- Module      :  Functions
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
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Functions (test_certify, test_apply,
                  makeDeltaRPM, readRPMSymbols, deltaRPMSymbols, readDeltaRPM, applyDeltaRPM,
                  getCommandLine1, getCommandLine2, installRPM, getSymbolsTable,
                  Internal (..)) where

import System.Directory
import System.Process
import System.Exit
import System.IO.Unsafe
import System.Console.CmdArgs.Verbosity
import Data.String.Utils

import Control.Monad
import Control.Applicative


import Data.Algorithm.Diff3
import Data.String.Utils
import Data.Typeable
import Data.Maybe
import Data.Char
import qualified Data.List as List

import Path
import Python
import Text.Parsec.String
import Text.Regex.Posix

import NMParser
import Extraction
import Safety
import InternalData
import System.Exit



#ifndef NO_RELEASE_ARCH
#define RELEASE "1"
#define ARCH "i686"
#endif

makeDeltaRPM
  :: Internal -> Internal
makeDeltaRPM (ValueList [RelFile a, RelFile b, RelDir c, RelFile d, Install i])
  = ValueList <$> unsafePerformIO $ (def (a,b,c,d) >>= \[a',b',c',d'] -> return [a', b', c', Install i])
    where def (old_rpm_path, new_rpm_path, workdir, delta_rpm_path)
            = do
              let cmd = "makedeltarpm " ++
                        (fromRelFile old_rpm_path) ++ " " ++
                        (fromRelFile new_rpm_path) ++ " " ++
                        (fromRelFile (workdir </> delta_rpm_path))
              whenLoud $ putStrLn $ "$>" ++ cmd
              (success, stdout, stderr) <- readCreateProcessWithExitCode (shell cmd) ""
              when (not (success == ExitSuccess))
                $ putStrLn ("makedeltarpm := stderr :" ++ stderr)
              return [RelFile old_rpm_path,
                      RelFile new_rpm_path,
                      RelDir workdir,
                      RelFile delta_rpm_path]

-- to be used on by the checker
readDeltaRPM
  :: Internal ->
     Internal
readDeltaRPM (ValueList [RelFile d, RelDir c, Install i])
  = ValueList <$> unsafePerformIO $
                  (def >>= \[a,b] -> return [RelFile a, RelFile b, RelFile d, RelDir c, Install i])
    where def = do
                let f = defVV $ "def export(x): from deltarpm import readDeltaRPM;" ++
                                "d = readDeltaRPM (x);" ++ -- ('" ++ c ++ "' + x);" ++
                                "d1 = d['old_nevr'];" ++
                                "d2 = d['nevr'];" ++
                                "return (d1,d2)"
                (a::Path Rel File, b::Path Rel File) <- f (fromRelFile (c </> d))
                let a_rel:'-':a' = reverse (fromRelFile a)
                    oldrpmdir = reverse a'
                    b_rel:'-':b' = reverse (fromRelFile b)
                    newrpmdir = reverse b'

                -- get the path for the new RPM
                let new_release = intToDigit $ 1 + digitToInt b_rel
                    new_rpm_name = newrpmdir ++ "-" ++ new_release:[] ++ "." ++ ARCH ++ ".rpm"
                nevr_path :: Path Rel File <- parseRelFile new_rpm_name
                let nevr :: Path Rel File = (c </> nevr_path)

                -- get the path for the old RPM
                let old_rpm_name = oldrpmdir ++ "-" ++ RELEASE ++ "." ++ ARCH ++ ".rpm"
                old_rpm_path <- findFile [oldrpmdir] old_rpm_name
                when (not (isJust old_rpm_path))
                   (error ("cannot read \"old_nevr\" from delta RPM"))

                old_nevr::Path Rel File <- parseRelFile (fromJust old_rpm_path)
                putStrLn $ "Delta RPM header := " ++ show (old_nevr, nevr)
                return [old_nevr, nevr]
-- to be used on by the checker

-- to be used on by the checker
applyDeltaRPM
    :: Internal ->
       Internal
applyDeltaRPM (ValueList [RelFile oldrpm, RelFile newrpm, RelFile deltarpm, RelDir c, Install i])
    = ValueList <$> unsafePerformIO $ def (c, deltarpm, oldrpm, newrpm, i)
      where def (c, d, o, n, i)
              = do
                let options = " -p -v -r " ++ fromRelFile o  ++ " "
                let cmd = "applydeltarpm" ++ options ++ " " ++
                              (fromRelFile (c </> d)) ++ " " ++ (fromRelFile n)
                whenLoud $ (putStrLn $ "$>" ++ cmd)
                (success, stdout, stderr) <- readCreateProcessWithExitCode (shell cmd) ""
                when (not (success == ExitSuccess))
                      $ putStrLn ("makedeltarpm := stderr :" ++ stderr)
                return [RelFile oldrpm, RelFile newrpm, RelDir c, Install i]
-- to be used on by the checker

installRPM
    :: Internal ->
       Internal
installRPM (ValueList [RelFile _, RelFile rpm, RelFile _, RelDir _, Install i])
    = if i then ValueList <$> unsafePerformIO $ def (rpm)
           else ProofChecker (ExitSuccess)
      where def (r)
              = do
                let options = " -vh -U --force "
                let cmd = "sudo rpm" ++ options ++ fromRelFile rpm
                whenLoud $ (putStrLn $ "$>" ++ cmd)
                (success, stdout, stderr) <- readCreateProcessWithExitCode (shell cmd) ""
                when (not (success == ExitSuccess))
                      $ putStrLn ("install := stderr :" ++ stderr)
                return []

readRPMSymbols
  :: Internal ->
     Internal
readRPMSymbols (ValueList [RelFile a, RelFile b, RelDir c, Install i])
  = let f (ll, lr) = [Table ll, Table lr, Install i]
    in ValueList <$> unsafePerformIO $ liftM f (def (a,b,c))
    where def (old, new, workdir)
            = readSymTab (workdir, old) False >>= \old_syms ->
                    readSymTab (workdir, new) True >>= \new_syms ->
                        return (map (\(a,b) -> Symbol a b) old_syms,
                                map (\(a,b) -> Symbol a b) new_syms)

readSymTab
    :: (Path Rel Dir, Path Rel File) ->
       Bool ->
       IO [(Char,String)]
readSymTab (workdir, rpm) new
    = do
      -- extract the RPM
      let cmd1 = "rm -rf ./usr ; rpm2cpio " ++ (fromRelFile rpm) ++ " | cpio --quiet -idmv 2>&1"
      whenLoud $ (putStrLn $ "$>" ++ cmd1)
      (success, stdout, stderr) <- readCreateProcessWithExitCode (shell cmd1) ""
      when (not (success == ExitSuccess)) $ putStrLn ("rpm2cpio := stderr :" ++ stderr)
      --whenLoud $putStrLn ("rpm2cpio := stdout :" ++ stdout)
      -- extract the binary file
      let b = head (filter (List.isInfixOf "bin") (lines stdout))
      let cmd2 = "nm " ++ b
      whenLoud $ (putStrLn $ "$>" ++ cmd2)
      -- parse the output of "nm"
      (success, stdout, stderr) <- readCreateProcessWithExitCode (shell cmd2) ""
      when (not (success == ExitSuccess)) $ putStrLn ("nm := stderr :" ++ stderr)
      --whenLoud $ putStrLn ("nm := stdout :" ++ stdout)
      let Right symbols = parse nmParser stdout
      -- resolve undefined symbols according to the safety policy
      safety_level <- getSafetyLevel
      whenLoud $ (putStrLn $ "Safety Level: " ++ (show safety_level))
      if (safety_level == Level1)
         then return $ map (\(a,b,c) -> (b,c)) symbols
         else do
              undefined_ <- resolveUndefinedSym b
              let f = (\s -> (not (startswith "found" s)))
                  open = filter f undefined_
              whenLoud $ putStrLn ("lookup symbols (stdout) := " ++ (show (length undefined_)))
              whenLoud $ putStrLn ("undefined symbols (stdout) := \n" ++ (show (undefined_)))
              mapM (\(a,b,c) -> do --whenLoud $ putStrLn ("Symbol:" ++ show c ++ " > " ++ show b)
                                   if (not (elem c undefined_) && b=='U' && new)
                                      then return ('?', c)
                                      else return (b, c)) symbols

resolveUndefinedSym
    :: String ->
       IO [String]
resolveUndefinedSym binary
    = do
      let cmd = "target=" ++ binary ++ ";" ++ "\n" ++
                "for symbol in $(nm -D $target | grep \"U \" | cut -b12-);" ++ "\n" ++
                "do for library in $(ldd $target | cut -d ' ' -f3- | cut -d' ' -f1);" ++ "\n" ++
                "do if [ \"$library\" != \"not\" ]; then" ++ "\n" ++
                "for lib_symbol in $(nm -D $library | grep \"T \" | cut -b12-);"  ++ "\n" ++
                "do if [ $symbol == $lib_symbol ]; then echo \"found $symbol in $library\"; fi ;"
                ++ "\n" ++
                "done;" ++ "\n" ++
                --"else echo \"missing $symbol\";" ++ "\n" ++
                "else echo \"$symbol\";" ++ "\n" ++
                "fi;" ++ "\n" ++
                "done;" ++ "\n" ++
                "done;"
      (ExitSuccess, stdout, stderr) <- readCreateProcessWithExitCode (shell cmd) ""
      whenLoud $ putStrLn ("undefined symbols (stdout) := \n" ++ stdout)
      let undefs = lines stdout
      return undefs

deltaRPMSymbols
  :: Internal ->
     Internal
deltaRPMSymbols (ValueList [Table a, Table b, Install i])
    = let f (Symbol s n) = (s,n)
          g = map (\(RightChange ((s,n):[])) -> Symbol s n)
          t = Table <$> unsafePerformIO $ liftM g (def (map f a, map f b))
      in ValueList (t:(Install i):[])
      where def (old, new)
              = do
                let diffs = diff3 old old new
                    f =  (\hunk -> case hunk of
                                        RightChange [] -> False
                                        RightChange (_:[]) -> True
                                        _ -> False)
                    result = filter f diffs
                {-putStrLn("==================================================")
                mapM (putStrLn . show) old
                putStrLn("==================================================")
                mapM (putStrLn . show) new
                putStrLn("==================================================")
                mapM (putStrLn . show) result
                putStrLn("==================================================") -}
                return result

getSymbolsTable       :: Internal -> Internal
getSymbolsTable args  = deltaRPMSymbols (readRPMSymbols args)


getCommandLine1
  :: FilePath ->
     FilePath ->
     FilePath ->
     FilePath ->
     IO Internal
getCommandLine1 oldrpmdir newrpmdir workdir deltarpm
   =  do
      -- get the path for the old RPM
      let old_rpm_name = oldrpmdir ++ "-" ++ RELEASE ++ "." ++ ARCH ++ ".rpm"
      old_rpm_path <- findFile [oldrpmdir] old_rpm_name
      when (not (isJust old_rpm_path))
           (error ("option \"oldrpmdir\" is invalid"))
      -- get the path for the new RPM
      let new_rpm_name = newrpmdir ++ "-" ++ RELEASE ++ "." ++ ARCH ++ ".rpm"
      new_rpm_path <- findFile [newrpmdir] new_rpm_name
      when (not (isJust new_rpm_path))
           (error ("option \"newrpmdir\" is invalid"))

      workdir  :: Path Rel Dir  <- parseRelDir workdir
      deltarpm :: Path Rel File <- parseRelFile deltarpm
      old_rpm_path :: Path Rel File <- parseRelFile (fromJust old_rpm_path)
      new_rpm_path :: Path Rel File <- parseRelFile (fromJust new_rpm_path)

      return $ ValueList [RelFile old_rpm_path,
                          RelFile new_rpm_path,
                          RelDir workdir,
                          RelFile deltarpm,
                          Install False ]

getCommandLine2
  :: FilePath ->
     FilePath ->
     Bool ->
     IO Internal
getCommandLine2 workdir deltarpm install
   =  do
      workdir  :: Path Rel Dir  <- parseRelDir workdir
      deltarpm :: Path Rel File <- parseRelFile deltarpm


      return $ ValueList [RelFile deltarpm,
                          RelDir workdir,
                          Install install ]

test_certify
    :: FilePath ->
       FilePath ->
       FilePath ->
       FilePath ->
       IO ()
test_certify oldrpmdir newrpmdir workdir deltarpm
    = do
      args <- getCommandLine1 oldrpmdir newrpmdir workdir deltarpm
      let ValueList [RelFile old_rpm_path,
                     RelFile new_rpm_path,
                     RelDir workdir,
                     RelFile deltarpm ] = args

      -- invoke untrusted "makedeltarpm"
      let delta_rpm_path = makeDeltaRPM args

      -- invoke "python read deltarpm"
      let old_nevr_nevr = readDeltaRPM $ ValueList [RelDir workdir, RelFile deltarpm]
      whenLoud $ putStrLn ("read: " ++ show old_nevr_nevr)

      -- read the table of symbols in both RPMS
      let syms = readRPMSymbols $
                    ValueList [RelFile old_rpm_path,
                               RelFile new_rpm_path,
                               RelDir workdir]

      -- read the difference between the RPMS
      let Table hunks = deltaRPMSymbols syms
      mapM (\h -> whenLoud $ (putStrLn . show) h) hunks
      return ()


test_apply
  :: FilePath ->
     FilePath ->
     Bool ->
     IO ()
test_apply workdir deltarpm install
  = do
    args <- getCommandLine2 workdir deltarpm install
    let ValueList [RelFile deltarpm,
                   RelDir workdir ] = args
    whenLoud $ putStrLn ("read: " ++ show args)

    -- invoke "python read deltarpm"
    let old_nevr_nevr = readDeltaRPM $ args
    whenLoud $ putStrLn ("read: " ++ show old_nevr_nevr)

    -- invoke applydeltarpm
    let args' = applyDeltaRPM $ old_nevr_nevr
    whenLoud $ putStrLn ("read: " ++ show args')

    -- read the table of symbols in both RPMS
    let syms = readRPMSymbols $ args'

    -- read the difference between the RPMS
    let Table hunks = deltaRPMSymbols syms
    mapM (\h -> whenLoud $ (putStrLn . show) h) hunks
    return ()

