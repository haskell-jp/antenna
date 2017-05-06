module Main where

import Antenna (app)
import Antenna.Config (getEnv)

main :: IO ()
main = app =<< getEnv

