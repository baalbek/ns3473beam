{-# LANGUAGE CPP,NamedFieldPuns,RecordWildCards #-}
module NS3473Beam.System where
-- module NS3473Beam.System (BeamSystem(..),runSystem) where

import Data.Maybe (fromJust)

import Control.Monad.Writer (Writer,runWriter,tell,writer)

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
                    w      :: Double,  -- ^ Beam width 
                    h      :: Double,  -- ^ Beam height
                    rd     :: Double,  -- ^ Rebar diam  
                    rmnt   :: Int,     -- ^ Rebar amount
                    rlay   :: Int,     -- ^ Number of rebar layers
                    shear  :: Maybe Double, 
                    moment :: Maybe Double
                } deriving Show


vccdCheck :: B.Beam  
            -> Maybe Double  -- ^ Shear 
            -> Maybe Double  -- ^ Moment 
            -> Writer String Bool
vccdCheck beam v m = let valOrZero x = case x of Nothing -> 0.0 
                                                 Just x' -> x'
                         m' = valOrZero m
                         v' = valOrZero v
                         vccd = B.vccd beam m' 
                         result = vccd > v'
    in writer (result,printf "[Shear] Vccd: %.2f, shear: %.2f" vccd v')

mcdCheck :: B.Beam  
            -> Maybe Double  -- ^ Moment 
            -> Writer String Bool
mcdCheck beam m = 
    let 
        hasMoment m =  
            let mcd = B.mcd beam 
                Just m' = m 
                mcdWriter bo = writer (bo, printf "[Beam] Mcd: %.2f kNm, dim moment: %.2f, diff: %.2f kNm" mcd m' (mcd - m')) 
            in case (mcd > m') of 
                True -> mcdWriter True
                False -> mcdWriter False
        noMoment m = 
            writer (True, "Dim. moment is 0.0")
    in case (m == Nothing) of
        True -> noMoment m
        False -> hasMoment m

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
ccLinksCheck beam v m = 
    let diam = linkDiam beam
        minCc = B.minCcLinks beam
        cc = ccLinksOrDefault beam v m minCc in 
    writer (True, (printf "[Links %.0f mm] Min. cc: %.2f mm, cc: %.2f mm" diam minCc cc))

tensileRebarCheck :: B.Beam 
                     -> Maybe C.StaticMoment
                     -> Writer String Bool
tensileRebarCheck beam m = 
    let hasMoment m = let Just m' = m
                          mfAs' = B.mfAs beam m'  
                          rebars = B.rebars beam
                          curAs = R.totalSteelArea rebars 
                          asOk = curAs > mfAs'
                          rebarDiam = R.diam (R.rebar rebars)
                      in writer (asOk, printf "[Tensile Rebar %.0f mm] Required: %.2f mm2, current: %.2f mm2" rebarDiam mfAs' curAs) 
    in if m == Nothing 
            then 
                writer (True, "[Tensile Rebar Check] Dim. moment is 0.0") 
            else
                hasMoment m

displayResult :: (Bool,String) -> IO ()
displayResult r =  
     --mapM_ (putStrLn . ("\t"++)) (fromDiffList $ snd r)
    printf "\t%s\n" (snd r) >> 
    return ()

createBeam :: BeamSystem -> B.Beam
createBeam bs = B.RectBeam (w bs) (h bs) myConc myRebar (B.Link 8)
    where myRebar | rlay' == 1 = R.SingleRowBeamRebars rebar rmnt' 25
                  | otherwise = R.MultiRowBeamRebars rebar rmnt' rlay' 25 25
          myConc = M.newConc "35" 
          rd' = rd bs
          rebar = R.Rebar rd'
          rmnt' = fromIntegral $ rmnt bs
          rlay' = fromIntegral $ rlay bs

runSystem :: BeamSystem -> IO ()
runSystem bs =
    printf "System %s\n" (show bs) >>
    let beam = createBeam bs
        m = moment bs 
        v = shear bs
        passedChecks what x = (fst x) == what
        results = [runWriter (vccdCheck beam v m), runWriter (tensileRebarCheck beam m), runWriter (mcdCheck beam m), runWriter (ccLinksCheck beam v m)] in 
    putStrLn "Passed:" >> 
    mapM_  displayResult (filter (passedChecks True) results) >> 
    putStrLn "Failed:" >> 
    mapM_  displayResult (filter (passedChecks False) results) >>
    return ()
