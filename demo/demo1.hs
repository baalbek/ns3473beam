import qualified NS3473.Beams as B
import qualified NS3473Beam.System as S

b = S.BeamSystem 450 300 16 8 1 Nothing (Just 135)

b2 = S.BeamSystem 200 500 12 4 1 (Just 100) (Just 90.4)

b3 = S.BeamSystem 1000 150 12 4 1 (Just 100) (Just 35)
b3x = S.createBeam b3

b4 = S.BeamSystem 1000 150 12 5 1 Nothing (Just 28)

b5 = S.BeamSystem 200 500 12 9 2 Nothing (Just 28)

{-
b3 = S.BeamSystem 200 500 Nothing (Just 300)

b4 = S.BeamSystem 200 500 (Just 700) (Just 300)

b5 = S.BeamSystem 200 500 (Just 8000) Nothing
-}

