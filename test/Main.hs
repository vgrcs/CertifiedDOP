-----------------------------------------------------------------------------
--
-- Module      :  Profiler
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

module Main ( main

) where

import Criterion.Main
import System.Process

main =  do
        let cmd = "DeltaCert apply -v -r -d ecall_delta-1.0-1.i686.drpm"
            prog = readCreateProcessWithExitCode (shell cmd) ""
        defaultMain [ bgroup "checker"  [ bench "10" $ whnfIO prog ] ]
