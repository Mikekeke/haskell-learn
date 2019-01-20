import Control.Arrow
import Control.Category
import Prelude hiding (id,(.))

newtype SimpleFunc a b = SimpleFunc { runF :: (a -> b) }

instance Arrow SimpleFunc where
    arr f = SimpleFunc f
    first (SimpleFunc f) = SimpleFunc (mapFst f)
                       where mapFst g (a,b) = (g a, b)
    second (SimpleFunc f) = SimpleFunc (mapSnd f)
                       where mapSnd g (a,b) = (a, g b)
    
instance Category SimpleFunc where
        (SimpleFunc g) . (SimpleFunc f) = SimpleFunc (g . f)
        id = arr id

tstF1 = SimpleFunc (+10)