module Chess where
import System.Random

type Pos = (Int, Int)
delta :: [Pos]
delta = [ ((-2), (-1)), ((-2), 1), (2, (-1)), (2, 1), ((-1), (-2)), ((-1), 2), (1, (-2)), (1, 2) ]

addPos :: Pos -> Pos -> Pos
addPos (x1, y1) (x2, y2) = (x1+x2, y1+y2)

isValid :: Pos -> Bool
isValid (x,y) = x >= 1 && x <= 8 && y >= 1 && y <= 8

positions :: Pos -> [Pos]
positions = filter isValid . flip map delta . addPos

nextPositions :: [Pos] -> [Pos]
nextPositions (current:xs) = filter (flip notElem xs) (positions current)
nextPositions [] = []

takeIndex :: Int -> [Pos] -> Maybe Pos
takeIndex _ [] = Nothing
takeIndex idx list = Just $ list!!(idx `mod` length list)

nextPosition :: Int -> [Pos] -> Maybe [Pos]
nextPosition idx l = fmap (flip (:) l) $ takeIndex idx (nextPositions l)

positionList :: [Pos] -> [Int] -> [Pos]
positionList l [] = l
positionList l (i:is) = case (nextPosition i l) of
  Just nl -> positionList nl is
  Nothing -> l

randomPos :: StdGen -> (Pos, StdGen)
randomPos xgen =
  let (x, ygen) = randomR (1,8) xgen
      (y, zgen) = randomR (1,8) ygen
  in ((x,y),zgen)

randomList :: IO ([Pos])
randomList = do
  countGen <- getStdGen
  let (count, posGen) = randomR (10,20) countGen
  let (pos, indexGen) = randomPos posGen
  let indices = take count (randomRs (0,7) indexGen)
  setStdGen indexGen -- TODO: get a gen back from randomRs instead of recycling
  return $ positionList [pos] indices
