import           Control.Monad.Writer
import           Data.List
import           Data.Monoid

type Shopping = Writer (Sum Integer) ()

purchase :: String -> Integer -> Shopping
purchase item cost = tell $ Sum cost
-- purchase _ = tell . Sum

-- purchase' :: MonadWriter w m => t -> w -> m ()
-- purchase' item cost = tell cost

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

type Shopping3 = Writer (Sum Integer, [String]) String

purchase3 :: String -> Integer -> Shopping3
purchase3 item cost = tell ((Sum cost), [item]) >> return (head item : show cost)

toOrderId = intercalate "-"

shopping3 :: Shopping3
shopping3 = do
  id1 <- purchase3 "Jeans"   19200
  id2 <- purchase3 "Water"     180
  id3 <- purchase3 "Lettuce"   328
  return $ toOrderId [id1, id2, id3]
