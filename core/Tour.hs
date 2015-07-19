module Tour where
import Knight (Pos,validNextPositionsFor,isValidFor)
import Tree
import Data.List
import Data.Ord

-- Data.List (?)
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = map snd . sortBy (comparing fst)
                   . map (\x -> let y = f x in y `seq` (y, x))

-- Build Tour
nextUniquePositions :: (Pos -> Bool) -> [Pos] -> Pos -> [Pos]
nextUniquePositions isValid path p = filter (flip notElem path) validPositions
  where validPositions = validNextPositionsFor isValid p

-- Build Tour: Brute force
bruteForce :: (Pos -> Bool) -> [Pos] -> Pos -> [Pos]
bruteForce = nextUniquePositions

-- Build Tour: Warnsdorf
warnsdorf :: (Pos -> Bool) -> [Pos] -> Pos -> [Pos]
warnsdorf isValid path p = sortOn (length . getNext) next
  where getNext = validNextPositionsFor isValid
        next = nextUniquePositions isValid path p

-- Filter Tour
firstDepthIO :: Int -> Int -> [[Pos]] -> IO (Maybe [Pos])
firstDepthIO _ _ [] = return Nothing
firstDepthIO i depth (x:xs) = do
  putStrLn $ show i
  if length x >= depth
    then putStrLn "FOUND" >> return (Just x)
    else firstDepthIO (i+1) depth xs

getTour :: Int -> Int -> Pos -> Maybe [Pos]
getTour mx my p =
  let strategy = warnsdorf
      tree     = buildTree (strategy (isValidFor mx my)) [] p
  in firstToDepth (mx*my) tree

getTourIO :: Pos -> IO(Maybe [Pos])
getTourIO p = do
  let strategy = warnsdorf
  let tree = buildTree (strategy (isValidFor 8 8)) [] p
  let paths = mapTreePath id [] tree
  firstDepthIO 0 64 paths
