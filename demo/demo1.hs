import qualified NS3473.Beams as B
import qualified NS3473.Rebars as R
import qualified NS3473.Concrete as M
import qualified NS3473Beam.System as S


ctx = B.DeflectionContext 0.5 7000 1.3 M.ee 1.5

conc = M.newConc "25"

r = R.MultiRowBeamRebars (R.Rebar 20)  20 1 40 33

b = B.RectBeam 100 100 conc r (B.Link 8)

ee = B.een M.ee b 

eelt = B.een M.eeLt b 

defl = S.deflectionCheck b ctx (Just 36)

mcd = B.mcd b
