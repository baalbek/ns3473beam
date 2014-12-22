{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses,DeriveDataTypeable #-}

import Text.Printf (printf)

import Control.Monad

import System.Console.CmdLib -- (Attributes,Group,Help,ArgHelp,Default,RecordCommand)

import qualified NS3473Beam.System as S

data Main = Main { 
        s :: String,
        m :: String,
        b :: String, 
        bt :: String, 
        h :: String, 
        ht :: String, 
        c :: String, 
        d :: String, 
        o :: String,
        hlaydist :: String, 
        vlaydist :: String, 
        numlayers :: String, 
        linkdiam :: String 
    }
    deriving (Typeable, Data, Eq)

instance Attributes Main where
    attributes _ = group "Options" [
            s      %> [ Group "Krefter", Help "Dimensjonerende skjaerkraft (kN)", ArgHelp "VAL", Default "0.0" ], 
            m      %> [ Group "Krefter", Help "Dimensjonerende moment (kNm)", ArgHelp "VAL", Default "0.0" ],
            b      %> [ Group "Geometri, paakrevet", Help "Bjelkebredde (mm)", ArgHelp "VAL" ], 
            bt      %> [ Group "Geometri, tillegg", Help "Bjelkebredde flens (mm)", ArgHelp "VAL", Default "0.0" ], 
            h      %> [ Group "Geometri, paakrevet", Help "Bjelkehoyde (mm)", ArgHelp "VAL" ], 
            ht      %> [ Group "Geometri, tillegg", Help "Bjelkehoyde flens, platetykkelse (mm)", ArgHelp "VAL", Default "0.0" ], 
            c      %> [ Group "Materialegenskaper", Help "Betongkvalitet", ArgHelp "VAL", Default "35" ], 
            d      %> [ Group "Armering", Help "Armeringsdiameter (mm)", ArgHelp "VAL", Default "12.0" ], 
            o      %> [ Group "Armering", Help "Overdekning (mm)", ArgHelp "VAL", Default "25" ],
            hlaydist      %> [ Group "Armering", Help "Horisontal distanse mellom armeringslag (mm)", ArgHelp "VAL", Default "25" ], 
            vlaydist      %> [ Group "Armering", Help "Vertikal distanse mellom armeringslag (mm)", ArgHelp "VAL", Default "40" ], 
            numlayers      %> [ Group "Armering", Help "Antall armeringslag", ArgHelp "VAL", Default "1" ], 
            linkdiam      %> [ Group "Armering", Help "Boylediameter (mm)", ArgHelp "VAL", Default "8" ] 
        ]

instance RecordCommand Main where
    mode_summary _ = "NS 3473 Beams"

main :: IO ()
main = getArgs >>= executeR Main {} >>= \opts -> do
    let curS = (read (s opts) :: Double) 
    let curM = (read (m opts) :: Double) 
    let curB = (read (b opts) :: Double) 
    let curH = (read (h opts) :: Double) 
    let curD = (read (d opts) :: Double) 
    S.runSystem $ S.BeamSystem curB curH curS curM
    return ()

