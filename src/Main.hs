{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses,DeriveDataTypeable #-}


import Text.Printf (printf)

import System.Console.CmdLib -- (Attributes,Group,Help,ArgHelp,Default,RecordCommand)

import qualified NS3473Beam.System as S
import qualified NS3473Beam.CmdLine as CL

main :: IO ()
main = getArgs >>= executeR CL.Main {} >>= \opts -> do
    case (CL.x opts) of 
        1 -> S.checkBeam opts
        2 -> S.calcXiFactor opts
    return ()


