import Knight

-- Tree stuff
data Tree a = Tree a [Tree a]
  deriving Show

mapTree :: (a -> b) -> Tree a -> [b]
mapTree fn (Tree a xs) = (fn a):(concat $ fmap (mapTree fn) xs)

mapTreeWithDepth :: (Int -> a -> b) -> Int -> Tree a -> [b]
mapTreeWithDepth fn depth (Tree a xs) = (fn depth a):(concat $ fmap (mapTreeWithDepth fn (depth+1)) xs)

mapTreePath :: ([a] -> b) -> [a]-> Tree a -> [b]
mapTreePath fn path (Tree a []) = [fn $ a:path]
mapTreePath fn path (Tree a xs) = concat $ fmap (mapTreePath fn (a:path)) xs

firstToDepth :: Int -> Tree a -> Maybe [a]
firstToDepth depth step = case pathsOfMatchingDepth of
    [] -> Nothing
    x:xs -> Just x
  where paths = mapTreePath id [] step
        pathsOfMatchingDepth = filter ((depth<=) . length) paths

-- Build Tour
nextStep :: [Pos] -> Pos -> Tree Pos
nextStep list p = Tree p steps
  where steps = fmap (nextStep $ p:list) (filter (flip notElem list) (validNextPositions p))

-- Show Tour
showNode :: Show a => Int -> a -> IO ()
showNode depth val = putStrLn $ (replicate depth ' ') ++ (show val)

--main = do
--  let t = nextStep [] (1,1)
--  sequence $ take 200 $ mapTreeWithDepth showNode 0 t

main = do
  let path = firstToDepth 57 $ nextStep [] (1,1)
  putStrLn $ maybe "No match" show path
  putStrLn $ maybe "No match" (show . length) path
