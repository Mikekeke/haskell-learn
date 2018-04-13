import Control.Monad.Writer
import Data.Monoid

type Shopping = Writer (Sum Integer) ()

purchase :: String -> Integer -> Shopping
purchase item cost = tell $ Sum cost
-- purchase _ = tell . Sum

purchase' :: MonadWriter w m => t -> w -> m ()
purchase' item cost = tell cost

total :: Shopping -> Integer
total = getSum . execWriter


shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328


type Shopping2 = Writer (Sum Integer, [String]) ()

purchase2 :: String -> Integer -> Shopping2
purchase2 item cost = tell ((Sum cost), [item])

total2 :: Shopping2 -> Integer
total2 = getSum . fst . execWriter

items :: Shopping2 -> [String]
items = snd . execWriter

shopping2 :: Shopping2
shopping2 = do
  purchase2 "Jeans"   19200
  purchase2 "Water"     180
  purchase2 "Lettuce"   328