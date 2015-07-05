module Knight where
import System.Random

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

randomPos :: StdGen -> (Pos, StdGen)
randomPos xgen = ((x,y),zgen)
  where (x, ygen) = randomR (1,8) xgen
        (y, zgen) = randomR (1,8) ygen

randomRn :: (Int, Int) -> Int -> StdGen -> ([Int], StdGen)
randomRn _ 0 gen = ([], gen)
randomRn range count gen = (r:rs, rsgen)
  where (r, rgen) = randomR range gen
        (rs, rsgen) = randomRn range (count-1) rgen

randomList :: IO ([Pos])
randomList = do
  countGen <- getStdGen
  let (count, posGen) = randomR (10,20) countGen
  let (pos, indexGen) = randomPos posGen
  let (indices, newGen) = randomRn (0,7) count indexGen
  setStdGen newGen
  return $ positionList [pos] indices
