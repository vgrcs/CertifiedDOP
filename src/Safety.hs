-----------------------------------------------------------------------------
--
-- Module      :  Safety
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

module Safety ( setSafetyLevel, getSafetyLevel, whenLevel_2, Safety (..)

) where

import Data.IORef
import System.IO.Unsafe
import Control.Monad

-- | The verbosity data type
data Safety
    = Level1 -- ^ Reject shared libraies
    | Level2 -- ^ Try to resolve undefined symbols in the "NEEDED" header
    deriving (Ord, Eq, Show)

{-# NOINLINE ref #-}
ref :: IORef Safety
ref = unsafePerformIO $ newIORef Level1

-- | Set the global safety level.
setSafetyLevel :: Safety -> IO ()
setSafetyLevel = writeIORef ref

-- | Get the global safety.
getSafetyLevel :: IO Safety
getSafetyLevel = readIORef ref

isSafeLevel_1 :: IO Bool
isSafeLevel_1 = fmap (>=Level1) getSafetyLevel

isSafeLevel_2 :: IO Bool
isSafeLevel_2 = fmap (>=Level2) getSafetyLevel

whenLevel_2 :: IO () -> IO ()
whenLevel_2 act = do
    b <- isSafeLevel_2
    when b act

