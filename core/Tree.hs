module Tree where

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
firstToDepth depth t = case pathsOfMatchingDepth of
    [] -> Nothing
    x:xs -> Just x
  where paths = mapTreePath id [] t
        pathsOfMatchingDepth = filter ((depth<=) . length) paths

buildTree :: ([a] -> a -> [a]) -> [a] -> a -> Tree a
buildTree fn path a = Tree a next
  where next = fmap (buildTree fn $ a:path) (fn path a)
