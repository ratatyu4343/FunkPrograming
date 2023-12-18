type Vertex = Int
type Edge = (Vertex, Vertex)
type Graph = [(Vertex, [Vertex])]

-- Функція для отримання списку всіх шляхів між двома вершинами в графі
allPaths :: Graph -> Vertex -> Vertex -> [[Vertex]]
allPaths graph start end = findPaths [start] []
  where
    findPaths path visited
      | last path == end = [path]
      | otherwise =
        let currentVertex = last path
            neighbors = filter (`notElem` visited) $ findNeighbors currentVertex graph
            extendedPaths = concatMap (\neighbor -> findPaths (path ++ [neighbor]) (visited ++ [currentVertex])) neighbors
        in extendedPaths

-- Функція для отримання сусідів даної вершини у графі
findNeighbors :: Vertex -> Graph -> [Vertex]
findNeighbors vertex graph = case lookup vertex graph of
  Just neighbors -> neighbors
  Nothing -> []

-- Приклад використання
main :: IO ()
main = do
  let exampleGraph :: Graph
      exampleGraph = [(1, [2, 3]), (2, [1, 3, 4]), (3, [1, 2, 4]), (4, [2, 3])]

      startVertex = 1
      endVertex = 4

  putStrLn $ "All paths from " ++ show startVertex ++ " to " ++ show endVertex ++ ":"
  mapM_ print $ allPaths exampleGraph startVertex endVertex
  
