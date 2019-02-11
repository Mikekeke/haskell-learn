import Data.Functor.Identity
import Data.Functor.Const

-- point :: Functor f => (Point -> f Point) -> Atom -> f Atom
-- point k atom = fmap (\newPoint -> atom { _point = newPoint }) (k (_point atom))

data Coordinate = Coord {lat :: Double, lon :: Double} deriving Show
data Location = Location {x :: Coordinate, y :: Coordinate} deriving Show

testLoc = Location {
    x = Coord 10 10
    , y = Coord 20 20
}

latLens :: Functor f => (Coordinate -> f Coordinate) -> Location -> f Location
latLens k loc = fmap (\newCoord -> loc {x = newCoord}) (k (x loc))
change = latLens Coord 1

get l = getConst . (l Const)
modify l = runIdentity . (l Identity)
set l loc = modify l (const loc)