{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses,DeriveDataTypeable #-}


import Text.Printf (printf)

import System.Console.CmdArgs (cmdArgs,Data,Typeable,typ,def,groupname,(&=))

import qualified NS3473Beam.System as S
import qualified NS3473Beam.CmdLine as CL

main :: IO ()
main = cmdArgs CL.cmdLine >>= \opts -> 
    putStrLn (CL.f opts) >>
    return ()


{-
main = getArgs >>= executeR CL.Main {} >>= \opts -> do
    case (CL.x opts) of 
        1 -> S.checkBeam opts
        2 -> S.calcXiFactor opts
        3 -> S.showD opts
        4 -> S.showFlangeDratio opts
    return ()

-}

