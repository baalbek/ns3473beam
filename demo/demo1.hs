import qualified NS3473.Beams as B
import qualified NS3473Beam.System as S

b = S.BeamSystem 200 500 (Just 50) (Just 100)

b2 = S.BeamSystem 200 500 (Just 50) Nothing

b3 = S.BeamSystem 200 500 Nothing (Just 300)

b4 = S.BeamSystem 200 500 (Just 700) (Just 300)

b5 = S.BeamSystem 200 500 (Just 8000) Nothing

