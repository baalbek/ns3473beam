{-# LANGUAGE CPP,NamedFieldPuns,RecordWildCards #-}
module NS3473Beam.System (BeamSystem(..),runSystem) where

import Data.Maybe (fromJust)

import Control.Monad.Writer (Writer,runWriter,tell)

import Text.Printf (printf)

import Data.Monoid ((<>))

import Text.Printf (printf)

import qualified NS3473.Common as C
import qualified NS3473.Concrete as M
import qualified NS3473.Rebars as R
import qualified NS3473.Beams as B
import NS3473.DiffList (DiffList,toDiffList,fromDiffList)

type StringDL = DiffList String 

data BeamSystem = BeamSystem {
                    w      :: Double, -- ^ Beam width 
                    h      :: Double,  -- ^ Beam height
                    shear  :: Maybe Double, 
                    moment :: Maybe Double
                } deriving Show

mcdCheck :: B.Beam  
            -> Maybe Double  -- ^ Moment 
            -> Writer String Bool
mcdCheck beam m = do 
    let hasMoment m = do 
        let mcd = B.mcd beam
        let Just m' = m
        tell $ printf "[Beam] Mcd: %.2f kNm, dim moment: %.2f, diff: %.2f kNm" mcd m' (mcd - m')
        if mcd > m'
            then 
                return True
            else
                return False
    let noMoment m = do 
        tell "Dim. moment is 0.0"
        return True
    if m == Nothing 
        then 
            noMoment m
        else
            hasMoment m

ccLinksOrDefault :: B.Beam
                    -> Maybe Double  -- ^ Shear
                    -> Maybe Double  -- ^ Moment 
                    -> Double  -- ^ Default value if result == Nothing
                    -> Double
ccLinksOrDefault beam v m defaultValue | cc == Nothing = defaultValue 
                                       | otherwise = fromJust cc
    where cc | v == Nothing = Nothing
             | m == Nothing = Nothing
             | otherwise = B.ccLinks beam (fromJust m) (fromJust v)

rebarDiam :: B.Beam -> Double
rebarDiam = R.diam . R.rebar . B.rebars

linkDiam :: B.Beam -> Double
linkDiam = B.diam . B.links

ccLinksCheck :: B.Beam 
                -> Maybe Double  -- ^ Shear
                -> Maybe Double  -- ^ Moment 
                -> Writer String Bool
ccLinksCheck beam v m = do
    let diam = linkDiam beam
    let minCc = B.minCcLinks beam
    let cc = ccLinksOrDefault beam v m minCc
    tell $ printf "[Bøyler %.0f mm] Min. cc: %.2f mm, cc: %.2f mm\n" diam minCc cc
    return True

stretchRebarCheck :: B.Beam 
                     -> C.StaticMoment
                     -> Writer String Bool
stretchRebarCheck beam m = do
    return True

displayResult :: (Bool,String) -> IO ()
displayResult r = do
     --mapM_ (putStrLn . ("\t"++)) (fromDiffList $ snd r)
    printf "\t%s\n" (snd r)
    return ()

createBeam :: BeamSystem -> B.Beam
createBeam bs = B.defaultBeam (w bs) (h bs) 12 4



runSystem :: BeamSystem -> IO ()
runSystem bs = do
    printf "System %s\n" (show bs)
    let passedChecks what x = (fst x) == what
    let beam = createBeam bs
    let m = (moment bs)
    let v = (shear bs)
    let results = [runWriter (mcdCheck beam m), runWriter (ccLinksCheck beam v m)]
    putStrLn "OK:"
    mapM_  displayResult $ filter (passedChecks True) results
    putStrLn "Underdimensjonert:"
    mapM_  displayResult $ filter (passedChecks False) results
