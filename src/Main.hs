{-# LANGUAGE DeriveDataTypeable #-}

import Text.Printf (printf)

import System.Console.CmdArgs

import NS3473Beam.CmdLine (cmdLine)
import qualified NS3473Beam.System as S


main :: IO ()
main = cmdArgs cmdLine >>= \x -> 
    putStrLn (show x)

{-
main :: IO ()
main = getArgs >>= executeR CL.Main {} >>= \opts -> do
    case (CL.x opts) of 
        1 -> S.checkBeam opts
        2 -> S.calcXiFactor opts
        3 -> S.showD opts
        4 -> S.showFlangeDratio opts
    return ()
-}

