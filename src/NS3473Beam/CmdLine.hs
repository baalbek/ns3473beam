{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses,DeriveDataTypeable #-}
module NS3473Beam.CmdLine where

import System.Console.CmdArgs (Data,Typeable,typ,def,groupname,help,(&=))
import qualified NS3473Beam.BeamSystem as B 

data CmdLine = 
    CmdLine {
        factorsu :: Double,
        moment :: Double,
        vshear :: Double,
        width :: Int,
        wflange :: Int,
        nd :: Int,
        nl :: Int,
        lt :: Bool
        } deriving (Show, Data, Typeable)

cmdLine = CmdLine {
    factorsu = 1.4 &= groupname "Forces" &= help "Brudd/bruksgrensefaktor (brudd divideres m/ -f). Default: 1.4",
    moment = 0  &= groupname "Forces" &= help "Dimensjonerende moment (kNm). Default: 0",
    vshear = 0  &= groupname "Forces" &= help "Dimensjonerende skjærkraft (kN). Default: 0",
    width = 0  &= groupname "Geometry" &= help "Beam width (mm). Default: 0",
    wflange = 0  &= groupname "Geometry" &= help "Beam width flange (mm). Default: 0",
    nd = 10  &= groupname "Rebars" &= help "Number of Rebars. Default: 10",
    nl = 1  &= groupname "Rebars" &= help "Number of Rebar layers. Default: 1",
    lt = True  &= groupname "Deflection" &= help "Use long term emodulus for deflections. Default: True"
    }
    
{-
data Mainx = Mainx { 
        f :: String,
        v :: String,
        m :: String,
        b :: Int, 
        bt :: Int, 
        h :: Int, 
        ht :: Int, 
        c :: String, 
        d :: Int, 
        nd :: Int, 
        o :: Int,
        hd :: Int, 
        vd :: Int, 
        nl :: Int, 
        ld :: Int,
        s :: Int,
        x :: Int,
        lt :: Bool,
        xi :: String
    }
    deriving (Data, Typeable, Show)

instance Attributes Main where
    attributes _ = group "Options" [
            x      %> [ Group "Choice", Help "1: calculate beam\n2: calculate xi factor\n3: calculate deflection\n\nDefault: 1", ArgHelp "VAL", Default (1 :: Int) ], 
            f      %> [ Group "Krefter", Help "Brudd/bruksgrensefaktor (brudd divideres m/ -f)", ArgHelp "VAL", Default "1.3" ], 
            v      %> [ Group "Krefter", Help "Dimensjonerende skjaerkraft (kN)", ArgHelp "VAL", Default "0.0" ], 
            m      %> [ Group "Krefter", Help "Dimensjonerende moment (kNm)", ArgHelp "VAL", Default "0.0" ],
            b      %> [ Group "Geometri, paakrevet", Help "Bjelkebredde (mm)", ArgHelp "VAL" ], 
            bt      %> [ Group "Geometri, tillegg", Help "Bjelkebredde flens (mm) (default: 0)", ArgHelp "VAL", Default (0 :: Int) ], 
            h      %> [ Group "Geometri, paakrevet", Help "Bjelkehoyde (mm)", ArgHelp "VAL" ], 
            ht      %> [ Group "Geometri, tillegg", Help "Bjelkehoyde flens, platetykkelse (mm) (default: 0)", ArgHelp "VAL", Default (0 :: Int) ], 
            c      %> [ Group "Materialegenskaper", Help "Betongkvalitet", ArgHelp "VAL", Default "35" ], 
            d      %> [ Group "Armering", Help "Armeringsdiameter (mm) (default: 12)", ArgHelp "VAL", Default (12 :: Int) ], 
            nd     %> [ Group "Armering, paakrevet", Help "Antall armeringstenger", ArgHelp "VAL" ], 
            o      %> [ Group "Armering", Help "Overdekning (mm) (default: 25)", ArgHelp "VAL", Default (25 :: Int) ],
            hd      %> [ Group "Armering", Help "Horisontal distanse mellom armeringslag (mm) (default: 25)", ArgHelp "VAL", Default (25 :: Int) ], 
            vd      %> [ Group "Armering", Help "Vertikal distanse mellom armeringslag (mm) (default: 40)", ArgHelp "VAL", Default (40 :: Int) ], 
            nl      %> [ Group "Armering", Help "Antall armeringslag (defalt: 1)", ArgHelp "VAL", Default (1 :: Int) ], 
            ld      %> [ Group "Armering", Help "Boylediameter (mm) (default: 8)", ArgHelp "VAL", Default (8 :: Int) ], 
            s %> [ Group "Deflection", Help "Span width (mm)", ArgHelp "VAL", Default (0 :: Int) ], 
            lt %> [ Group "Deflection", Help "Use long term emodulus for deflections", ArgHelp "VAL", Default True ],
            xi %> [ Group "Deflection", Help "Emodulus factor", ArgHelp "VAL", Default "0.5" ]
        ]

-}
