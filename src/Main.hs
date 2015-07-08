{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses,DeriveDataTypeable #-}

import GHC.Float (float2Double)

import Text.Printf (printf)

import Control.Monad

import System.Console.CmdLib -- (Attributes,Group,Help,ArgHelp,Default,RecordCommand)

import qualified NS3473Beam.System as S

data Main = Main { 
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

main :: IO ()
main = getArgs >>= executeR Main {} >>= \opts -> do
    let i2d x = fromIntegral (x opts)
    let s2dd x dv | curval > 0.0 = curval
                  | otherwise = dv
            where curval = (read (x opts) :: Double) 
    let s2d x = read (x opts) :: Double
    let curV = s2d v 
    let curM = s2d m 
    let curXI = s2d xi 
    let curLT = s2d xi 
    let curS = i2d s 
    let curB = i2d b 
    let curH = i2d h 
    let curD = i2d d 
    let curO = i2d o 
    let curHlaydist = i2d hd
    let curVlaydist = i2d vd
    let curNumlayers = i2d nl
    let curLinkdiam = i2d ld 
    let system = S.BeamSystem curB curH curD (nd opts) (nl opts) curO (Just curV) (Just curM) curS curXI (lt opts)
    let curX = i2d x
    case curX of 
        1 -> S.checkBeam system 
        2 -> S.calcXiFactor system 
        3 -> S.calcDeflection system 
    return ()

