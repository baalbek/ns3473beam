{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses,DeriveDataTypeable #-}

import GHC.Float (float2Double)

import Text.Printf (printf)

import Control.Monad

import System.Console.CmdLib -- (Attributes,Group,Help,ArgHelp,Default,RecordCommand)

import qualified NS3473Beam.System as S

data Main = Main { 
        s :: String,
        m :: String,
        b :: Int, 
        bt :: Int, 
        h :: Int, 
        ht :: Int, 
        c :: String, 
        d :: Int, 
        nd :: Int, 
        o :: Int,
        hlaydist :: Int, 
        vlaydist :: Int, 
        numlayers :: Int, 
        linkdiam :: Int 
    }
    deriving (Typeable, Data, Eq)

instance Attributes Main where
    attributes _ = group "Options" [
            s      %> [ Group "Krefter", Help "Dimensjonerende skjaerkraft (kN)", ArgHelp "VAL", Default "0.0" ], 
            m      %> [ Group "Krefter", Help "Dimensjonerende moment (kNm)", ArgHelp "VAL", Default "0.0" ],
            b      %> [ Group "Geometri, paakrevet", Help "Bjelkebredde (mm)", ArgHelp "VAL" ], 
            bt      %> [ Group "Geometri, tillegg", Help "Bjelkebredde flens (mm)", ArgHelp "VAL", Default (0 :: Int) ], 
            h      %> [ Group "Geometri, paakrevet", Help "Bjelkehoyde (mm)", ArgHelp "VAL" ], 
            ht      %> [ Group "Geometri, tillegg", Help "Bjelkehoyde flens, platetykkelse (mm)", ArgHelp "VAL", Default (0 :: Int) ], 
            c      %> [ Group "Materialegenskaper", Help "Betongkvalitet", ArgHelp "VAL", Default "35" ], 
            d      %> [ Group "Armering", Help "Armeringsdiameter (mm)", ArgHelp "VAL", Default (12 :: Int) ], 
            nd     %> [ Group "Armering, paakrevet", Help "Antall armeringstenger", ArgHelp "VAL" ], 
            o      %> [ Group "Armering", Help "Overdekning (mm) (default: 25)", ArgHelp "VAL", Default (25 :: Int) ],
            hlaydist      %> [ Group "Armering", Help "Horisontal distanse mellom armeringslag (mm) (default: 25)", ArgHelp "VAL", Default (25 :: Int) ], 
            vlaydist      %> [ Group "Armering", Help "Vertikal distanse mellom armeringslag (mm)", ArgHelp "VAL", Default (40 :: Int) ], 
            numlayers      %> [ Group "Armering", Help "Antall armeringslag", ArgHelp "VAL", Default (1 :: Int) ], 
            linkdiam      %> [ Group "Armering", Help "Boylediameter (mm)", ArgHelp "VAL", Default (8 :: Int) ] 
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
    let curS = s2d s 
    let curM = s2d m 
    let curB = i2d b 
    let curH = i2d h 
    let curD = i2d d 
    let curO = i2d o 
    let curHlaydist = i2d hlaydist      
    let curVlaydist = i2d vlaydist      
    let curNumlayers = i2d numlayers
    let curLinkdiam = i2d linkdiam 
    S.runSystem $ S.BeamSystem curB curH curD (nd opts) (numlayers opts) (Just curS) (Just curM)
    return ()

