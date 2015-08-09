
module NS3473Beam.BeamSystem where

-- let system = S.BeamSystem curB curH curD (nd opts) (nl opts) curO (Just curV) (Just curM) curS curXI (lt opts)

class BeamSystem a where 
    w         :: a -> Double    -- ^ Beam width [mm]
    wt        :: a -> Double    -- ^ Beam width [mm]
    h         :: a -> Double    -- ^ Beam height [mm]
    ht        :: a -> Double    -- ^ Beam height [mm]
    isTProfile :: a -> Bool
    moment    :: a -> Maybe Double 
    shear     :: a -> Maybe Double
    rebarDiam :: a -> Double    -- ^ Tensile rebars [mm]
    numLay    :: a -> Double    -- ^ Number of tensile rebar layers
    numRebars :: a -> Double    -- ^ Number of tensile rebars 
    linksDiam :: a -> Double    -- ^ Diamer of links [mm]
    cover     :: a -> Double    -- ^ Distance from edge of outer tensile rebar to beam edge (typically: links diameter and concrete cover)
    hdist   :: a -> Double    -- ^ Horizontal distance between rebar layers [mm]
    vdist   :: a -> Double    -- ^ Horizontal distance between rebar layers [mm]
    span    :: a -> Double    -- ^ Beam span (length) [mm]
    --xi      :: a -> Double    -- ^ Emodulus factor
    f       :: a -> Double    -- ^ Ultimate limit to service limit factor
