{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module NS3473Beam.System where
-- module NS3473Beam.System (BeamSystem(..),runSystem) where

import Data.Maybe (fromJust)

import Control.Monad.Writer (Writer,runWriter,tell,writer)

import Text.Printf (printf)

import Data.Monoid ((<>))

import qualified NS3473.Common as C
import qualified NS3473.Concrete as M
import qualified NS3473.Rebars as R
import qualified NS3473.Beams as B
import qualified NS3473Beam.CmdLine as CL
import qualified NS3473Beam.BeamSystem as BS

import NS3473.DiffList (DiffList,toDiffList,fromDiffList)

type StringDL = DiffList String 

valOrZero :: Maybe Double -> Double
valOrZero x = case x of Nothing -> 0.0
                        Just x' -> x'

numVerticalRebarLayers :: CL.Main -> Int
numVerticalRebarLayers opts = div (CL.nd opts) (CL.nl opts)

vcdCheck :: B.Beam  
            -> Maybe Double  -- ^ Shear 
            -> Writer String Bool
vcdCheck beam v = let v' = valOrZero v
                      vcd = B.vcd beam
                      result = vcd > v'
    in writer (result,printf "[Shear] Vcd: %.2f, shear: %.2f" vcd v')

vccdCheck :: B.Beam  
            -> Maybe Double  -- ^ Shear 
            -> Maybe Double  -- ^ Moment 
            -> Writer String Bool
vccdCheck beam v m = let m' = valOrZero m
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
                mcdWriter bo = writer (bo, printf "[Moment] Mcd: %.2f kNm, dim moment: %.2f, diff: %.2f kNm" mcd m' (mcd - m')) 
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
             | otherwise = let m' = valOrZero m in B.ccLinks beam m' (fromJust v)

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

minAsCheck :: B.Beam 
              -> Writer String Bool
minAsCheck beam = 
    let minas = B.minAs beam 
        r8 = R.Rebar 8
        r10 = R.Rebar 10
        r12 = R.Rebar 12 
        nfn :: R.Rebar -> Double 
        nfn r = fromIntegral (ceiling (minas / (R.steelAreaRod r)))
        n8 = nfn r8 
        n10 = nfn r10
        n12 = nfn r12 in
    writer (True, (printf "[Min. as] %.0f mm2, Ø8=%.0f, Ø10=%.0f, Ø12=%.0f" minas n8 n10 n12))

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

beamWidthCheck :: CL.Main -> Writer String Bool
beamWidthCheck s = let cover = 25.0 
                       nvr = numVerticalRebarLayers s
                       rebarW = (fromIntegral nvr) * (BS.rebarDiam s)
                       concW = fromIntegral $ (nvr - 1) * 40 
                       totWidth = (2*cover) + rebarW + concW
                       w' = BS.w s 
                       widthOk = totWidth <= w'
                   in if widthOk == True
                        then 
                            writer (True, printf "[Dim %.0f mm] Beam width (%.0f mm) ok" totWidth w')
                        else 
                            writer (False, printf "[Dim %.0f mm] Beam width (%.0f mm) missing: %.0f mm" totWidth w' (totWidth-w'))

deflectionCheck :: B.Beam 
                   -> B.DeflectionContext 
                   -> Maybe C.StaticMoment
                   -> Writer String Bool
deflectionCheck beam ctx m =
    let Just m' = m 
        serviceM = m' / (B.u2s ctx)
        tol = 400.0
        bl = B.beamLen ctx
        maxDv = bl / tol 
        curDv  = B.deflection beam ctx serviceM in 
    case curDv > maxDv of True -> writer (False, printf "[Deflection, max %.0f mm, %.2f kNm, span: %.0f] %.0f mm" maxDv serviceM bl curDv)
                          False -> writer (True, printf "[Deflection, max %.0f mm, %.2f kNm, span: %.0f] %.0f mm" maxDv serviceM bl curDv)


displayResult :: (Bool,String) -> IO ()
displayResult r =  
     --mapM_ (putStrLn . ("\t"++)) (fromDiffList $ snd r)
    printf "\t%s\n" (snd r) >> 
    return ()

createBeam :: CL.Main -> B.Beam
createBeam opts = case BS.isTProfile opts of True -> B.TProfile w' h' myConc myRebar (B.Link links') (BS.wt opts) (BS.ht opts)
                                             False -> B.RectBeam w' h' myConc myRebar (B.Link links')
                    where myRebar | rlay' == 1 = R.SingleRowBeamRebars rebar rmnt' cov'
                                  | otherwise = R.MultiRowBeamRebars rebar rmnt' rlay' 25 cov'
                          w' = BS.w opts
                          h' = BS.h opts 
                          links' = BS.linksDiam opts
                          myConc = M.newConc "35" 
                          rd' = BS.rebarDiam opts
                          rebar = R.Rebar rd'
                          rmnt' = BS.numRebars opts 
                          rlay' = BS.numLay opts 
                          cov' = BS.cover opts 

-------------------------------------------------------------------
-------------------- Main System functions ------------------------
-------------------------------------------------------------------
calcXiFactor :: CL.Main -> IO ()
calcXiFactor opts =
    let eeFn | (CL.lt opts) == True = M.eeLt
             | otherwise = M.ee 
        beam = createBeam opts
        xi = B.xiFact eeFn beam in 
    putStrLn (show beam) >>
    printf "\nXi factor %.8f\n" xi >>
    return ()
    
checkBeam :: CL.Main -> IO ()
checkBeam opts =
    let beam = createBeam opts
        m = BS.moment opts
        v = BS.shear opts
        eeFn | (CL.lt opts) == True = M.eeLt
             | otherwise = M.ee 
        dctx = B.DeflectionContext 0.0 (BS.span opts) (BS.f opts) eeFn 9.6
        passedChecks what x = (fst x) == what
        results = [runWriter (vccdCheck beam v m), 
                   runWriter (vcdCheck beam v), 
                   runWriter (minAsCheck beam), 
                   runWriter (tensileRebarCheck beam m), 
                   runWriter (deflectionCheck beam dctx m), 
                   runWriter (mcdCheck beam m), 
                   runWriter (beamWidthCheck opts), 
                   runWriter (ccLinksCheck beam v m)] in 
    printf "Beam: %s\n" (show beam) >>
    putStrLn "\n--------------------- Passed: ---------------------" >> 
    mapM_  displayResult (filter (passedChecks True) results) >> 
    putStrLn "\n--------------------- Failed: ---------------------" >> 
    mapM_  displayResult (filter (passedChecks False) results) >>
    return ()

showD :: CL.Main -> IO ()
showD opts =
    let beam = createBeam opts 
        d' = B.calcD beam in 
    printf "D: %.4f\n" d' >>
    return ()

showFlangeDratio :: CL.Main -> IO ()
showFlangeDratio opts =
    let beam = createBeam opts 
        d' = B.calcD beam 
        ht = B.beamHT beam 
        r = ht / d' in
    printf "Ratio: %.4f\n" r >>
    return ()



{-
calcDeflection :: CL.Main -> IO ()
calcDeflection opts =
    let ctx = B.DeflectionContext (xi bs) (s bs) 
        beam = createBeam bs
        Just m = (moment bs)
        dv = B.deflection beam ctx m in 
    printf "Deflection %.8f mm\n" dv >>
    return ()
  -}
    
