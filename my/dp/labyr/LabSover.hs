import Data.Set
import Debug.Trace

labFile = "./my/dp/labyr/lab1"
parseTerrain c | c == '.' = True
               | c == 'x' = False
               | otherwise = error $ "Invalid tarrain: " ++ [c]

parseLab :: [String] -> [[Bool]]
parseLab = fmap $ fmap parseTerrain

printLab :: [[Bool]] -> [String]
printLab = fmap show

validRawLab :: [String] -> Bool
validRawLab [] = error "Empty lab"
validRawLab lab = let lens = fmap length lab
                  in size (fromList lens) == 1

type Labyrinth = [[Bool]]
data Position = Position {xPos :: !Int, yPos :: !Int} deriving (Eq, Ord)

instance Show Position where
    show p = show $ (,) <$> xPos <*> yPos $ p

isLegalPos :: Labyrinth -> Position -> Bool
isLegalPos lab pos | y' >= length lab = False
                   | x' >= length (lab !! y') = False
                   | otherwise = lab !! y' !! x'
                   where x' = xPos pos
                         y' = yPos pos

stepUp p = Position (xPos p) ((yPos p) + 1)
stepDown p =  Position (xPos p) ((yPos p) - 1)
stepLeft p =  Position ((xPos p) - 1) (yPos p)
stepRight p =  Position ((xPos p) + 1) (yPos p)
nextPoss :: Position -> [Position]
nextPoss p = zipWith ($) [stepUp, stepDown, stepLeft, stepRight] (repeat p)
isExit :: Labyrinth -> Position -> Bool
isExit l p = let x' = xPos p
                 y' = yPos p
             in y' == 0 || y'+1 == length l || x' == 0 || x'+1 == length (l!!y')

findOuts :: Labyrinth -> Position -> [Position] -> [[Position]]
findOuts lab pos seen | not (isLegalPos lab pos) = error "Illegal start position"
                      | isExit lab pos = [[pos]]
                      | otherwise = do
                        let  legal = Prelude.filter (isLegalPos lab) (nextPoss pos)
                        let new = Prelude.filter (\pos -> not (elem pos seen)) legal
                        newPos <- new 
                        nextPoss <- findOuts lab newPos (pos:seen)
                        [pos : nextPoss]

solveLab lab startPos = findOuts lab startPos []

testLab = ["xxx"
         , "x.."
         , "x.x"
         , "..."
         ]
testParsed = parseLab testLab
testLabOut = reverse $ findOuts testParsed (Position 1 2) []

currentPos = Position 2 5
main :: IO ()
main = do
    lines' <- lines <$> readFile labFile
    case validRawLab lines' of
        True -> let 
                    lab = parseLab lines'
                    result = solveLab lab currentPos
                in mapM_ (putStrLn.show) result
        False -> putStrLn $ "Invalid lab\n" ++ (show lines')