{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses,DeriveDataTypeable #-}
module NS3473Beam.CmdLine where

import GHC.Float (float2Double)
import System.Console.CmdLib -- (Attributes,Group,Help,ArgHelp,Default,RecordCommand)
import qualified NS3473Beam.BeamSystem as B 

data Main = Main { 
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
    deriving (Typeable, Data, Eq)

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

instance RecordCommand Main where
    mode_summary _ = "NS 3473 Beams"

i2d :: Main -> (Main -> Int) -> Double 
i2d opts x = fromIntegral (x opts)

s2d :: Main -> (Main -> String) -> Double
s2d opts x = read (x opts) :: Double

instance B.BeamSystem Main where
    w opts = i2d opts b
    h opts = i2d opts h
    moment opts = Just (s2d opts m)
    shear opts = Just (s2d opts v)
    rebarDiam opts = i2d opts d  
    linksDiam opts = i2d opts ld
    cover opts = (i2d opts o) + (B.linksDiam opts)
    hdist opts = i2d opts hd 
    vdist opts = i2d opts vd
    span opts = i2d opts s
    xi opts = s2d opts xi
    numLay opts = i2d opts nl
    numRebars opts = i2d opts nd
    f opts = s2d opts f
