module Knight where
import System.Random
import Control.Monad.Random

type Pos = (Int, Int)
delta :: [Pos]
delta = [ ((-2), (-1)), ((-2), 1), (2, (-1)), (2, 1), ((-1), (-2)), ((-1), 2), (1, (-2)), (1, 2) ]

addPos :: Pos -> Pos -> Pos
addPos (x1, y1) (x2, y2) = (x1+x2, y1+y2)

isValid :: Pos -> Bool
isValid (x,y) = x >= 1 && x <= 8 && y >= 1 && y <= 8

validNextPositions :: Pos -> [Pos]
validNextPositions = filter isValid . flip map delta . addPos

uniqueNextPositions :: [Pos] -> [Pos]
uniqueNextPositions (current:xs) = filter (flip notElem xs) (validNextPositions current)
uniqueNextPositions [] = []

takeIndex :: Int -> [Pos] -> Maybe Pos
takeIndex _ [] = Nothing
takeIndex idx list = Just $ list!!(idx `mod` length list)

nextPosition :: Int -> [Pos] -> Maybe [Pos]
nextPosition idx l = fmap (flip (:) l) $ takeIndex idx (uniqueNextPositions l)

positionList :: [Pos] -> [Int] -> [Pos]
positionList l [] = l
positionList l (i:is) = case (nextPosition i l) of
  Just nl -> positionList nl is
  Nothing -> l
  -- using maybe l (flip positionList is) (nextPosition i l)
  -- although point free-er is not more readable

randomPos :: (RandomGen g) => Rand g Pos
randomPos = do
  x <- getRandomR (1,8)
  y <- getRandomR (1,8)
  return (x,y)

randomRn :: (RandomGen g) => (Int, Int) -> Int -> Rand g [Int]
randomRn _ 0 = return []
randomRn range count = do
  x <- getRandomR range
  xs <- randomRn range (count-1)
  return (x:xs)

randomBoard :: (RandomGen g) => Rand g [Pos]
randomBoard = do
  count <- getRandomR (10,20)
  pos <- randomPos
  indices <- randomRn (0,7) count
  return $ positionList [pos] indices

randomBoardIO :: IO ([Pos])
randomBoardIO = do
  gen <- getStdGen
  let (board, newGen) = runRand randomBoard gen
  setStdGen newGen
  return board