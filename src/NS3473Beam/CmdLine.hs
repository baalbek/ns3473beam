{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses,DeriveDataTypeable #-}
module NS3473Beam.CmdLine where

import System.Console.CmdArgs (Data,Typeable,typ,def,groupname,help,(&=))
import qualified NS3473Beam.BeamSystem as B 

data CmdLine = 
    CmdLine {
        factorsu :: Double,
        moment :: Double,
        vshear :: Double,
        width :: Double,
        height :: Double,
        qflange :: Double,
        zweb :: Double,
        anumr :: Int,
        enl :: Int,
        xlt :: Bool,
        ocover :: Double,
        diam :: Double,
        rhd :: Double,
        brvd  :: Double,
        links :: Double,
        concrete :: Double,
        spanwidth :: Double
        } deriving (Show, Data, Typeable)

cmdLine = CmdLine {
    factorsu = 1.4 &= groupname "Forces" &= help "Brudd/bruksgrensefaktor (brudd divideres m/ -f). Default: 1.4",
    moment = 0  &= groupname "Forces" &= help "Dimensjonerende moment (kNm). Default: 0",
    vshear = 0  &= groupname "Forces" &= help "Dimensjonerende skjærkraft (kN). Default: 0",
    width = 0  &= groupname "Geometry" &= help "Beam width (mm). Default: 0",
    height = 0  &= groupname "Geometry" &= help "Beam height (mm). Default: 0",
    qflange = 0  &= groupname "Geometry" &= help "Beam width flange (mm). Default: 0",
    zweb = 0  &= groupname "Geometry" &= help "Beam height web, plate thickness (mm). Default: 0",
    anumr = 10  &= groupname "Rebars" &= help "Number of Rebars. Default: 10",
    enl = 1  &= groupname "Rebars" &= help "Number of Rebar layers. Default: 1",
    ocover  = 25 &= groupname "Rebars" &= help "Use long term emodulus for deflections. Default: True",
    diam = 12 &= groupname "Rebars" &= help "Diameter of main rebars (mm). Default: 12",
    rhd = 25 &= groupname "Rebars" &= help "Horizontal distance between rebar layers (mm). Default: 25",
    brvd = 40  &= groupname "Rebars" &= help "Vertical distance between rebars (mm). Default: 40",
    links = 8  &= groupname "Rebars" &= help "Links diameter (mm). Default: 8",
    xlt = False &= groupname "Deflection" &= help "If set, do not use long term emodulus for deflections. Default: False",
    --xi = 0 &= groupname "Deflection" &= help "Emodulus factor. Default: 0.5",
    spanwidth = 0.0 &= groupname "Deflection" &= help "Span width (mm).Default: 0",
    concrete  = 25 &= groupname "Materials" &= help "Concrete class. Default: 35"
    }
    
i2d :: CmdLine -> (CmdLine -> Int) -> Double 
i2d opts x = fromIntegral (x opts)

instance B.BeamSystem CmdLine where
    w cl = width cl 
    wt cl = qflange cl 
    h cl = height cl 
    ht cl = zweb cl 
    isTProfile cl = (qflange cl) > 0
    moment cl = Just (moment cl)
    shear cl = Just (vshear cl)
    rebarDiam cl = diam cl 
    linksDiam cl = links cl 
    cover cl = ocover cl  -- Note, this is implicit + (B.linksDiam opts)
    hdist cl = rhd cl  
    vdist cl = brvd cl 
    span cl = spanwidth cl
    --xi opts = s2d opts xi
    numLay cl = i2d cl enl 
    numRebars cl = i2d cl anumr 
    f cl = factorsu cl 

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

instance B.BeamSystem Main where
    w opts = i2d opts b
    wt opts = i2d opts bt
    h opts = i2d opts h
    ht opts = i2d opts ht
    isTProfile opts = (bt opts) > 0
    moment opts = Just (s2d opts m)
    shear opts = Just (s2d opts v)
    rebarDiam opts = i2d opts d  
    linksDiam opts = i2d opts ld
    cover opts = (i2d opts o)  -- Nope, this is implicit + (B.linksDiam opts)
    hdist opts = i2d opts hd 
    vdist opts = i2d opts vd
    span opts = i2d opts s
    xi opts = s2d opts xi
    numLay opts = i2d opts nl
    numRebars opts = i2d opts nd
    f opts = s2d opts f
-}
