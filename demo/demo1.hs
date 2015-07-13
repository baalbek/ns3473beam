import qualified NS3473.Beams as B
import qualified NS3473.Rebars as R
import qualified NS3473.Concrete as M

conc = M.newConc "25"

r = R.MultiRowBeamRebars (R.Rebar 20) 9 2 40 33

b = B.RectBeam 350 500 conc r (B.Link 8)

ee = B.een M.ee b 

eelt = B.een M.eeLt b 

