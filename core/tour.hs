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
bruteForce = nextUniquePositions

-- Build Tour: Warnsdorf
warnsdorf :: (Pos -> Bool) -> [Pos] -> Pos -> [Pos]
warnsdorf isValid path p = sortOn (length . getNext) next
  where getNext = validNextPositionsFor isValid
        next = nextUniquePositions isValid path p

-- Filter Tour
withLength :: [Pos] -> (Int, [Pos])
withLength l = (length l,l)

firstDepthIO :: Int -> Int -> [(Int,[Pos])] -> IO [(Int,[Pos])]
firstDepthIO _ _ [] = return []
firstDepthIO i depth (x:xs) = do
  putStrLn $ show i
  if fst x >= depth
  then putStrLn "FOUND" >> return [x]
  else firstDepthIO (i+1) depth xs

-- Show Tour
showNode :: Show a => Int -> a -> IO ()
showNode depth val = putStrLn $ (replicate depth ' ') ++ (show val)

main = do
  let strategy = warnsdorf
  let tree = buildTree (strategy (isValidFor 8 8)) [] (1,5)
  let paths = mapTreePath withLength [] tree
  filteredPaths <- firstDepthIO 0 64 paths
  let match = head filteredPaths
  putStrLn $ show $ fst match
  putStrLn $ show $ snd match
