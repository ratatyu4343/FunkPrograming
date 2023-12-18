import Data.List (permutations)
import System.Random (randomRs, getStdGen, randomRIO)

type City = Int
type Tour = [City]
type Population = [Tour]

-- Граф представлення міст та їх відстаней
type Graph = [[Int]]

-- Функція для обчислення відстані між двома містами
distance :: Graph -> City -> City -> Int
distance graph city1 city2 = graph !! city1 !! city2

-- Функція для обчислення відстані по маршруту
tourDistance :: Graph -> Tour -> Int
tourDistance graph tour = sum $ zipWith (distance graph) tour (tail tour ++ [head tour])

-- Функція для створення випадкової популяції маршрутів
generatePopulation :: Int -> Int -> IO Population
generatePopulation size numCities = do
  gen <- getStdGen
  return $ take size $ permutations [0..numCities-1] -- Всі можливі перестановки міст

-- Функція для вибору індивіда за методом турніру
selectIndividual :: Population -> Graph -> IO Tour
selectIndividual population graph = do
  let tournamentSize = 3
  candidates <- take tournamentSize <$> shuffle population
  return $ minimum candidates
  where
    shuffle = fmap fst . head . filter (\(_, gen') -> gen' /= gen) . iterate (\(x, gen') -> shuffle' x gen') . flip (,) gen
    shuffle' xs gen' = let (n, gen'') = randomR (1, length xs) gen'
                          (ys, zs) = splitAt n xs
                      in ys ++ take (n-1) zs ++ [xs !! (n-1)] ++ drop n zs
    gen = mkStdGen 42 -- Зафіксоване початкове значення генератора випадкових чисел

-- Функція для кросоверу двох турів
crossover :: Tour -> Tour -> IO Tour
crossover tour1 tour2 = do
  start <- randomRIO (0, length tour1 - 1)
  end <- randomRIO (start, length tour1 - 1)
  let child = take start tour1 ++ drop start (take end tour2) ++ drop end tour1
  return $ if length (filter (`elem` child) [start..end]) /= length [start..end] then error "Crossover Error" else child

-- Функція для мутації туру
mutate :: Tour -> IO Tour
mutate tour = do
  pos1 <- randomRIO (0, length tour - 1)
  pos2 <- randomRIO (0, length tour - 1)
  let mutatedTour = tour // [(pos1, tour !! pos2), (pos2, tour !! pos1)]
  return mutatedTour

-- Головна функція для генетичного алгоритму
geneticAlgorithm :: Graph -> Int -> Int -> Int -> IO Tour
geneticAlgorithm graph populationSize numGenerations mutationRate = evolve initialPopulation numGenerations
  where
    initialPopulation = generatePopulation populationSize (length graph)

    evolve :: Population -> Int -> IO Tour
    evolve population 0 = return $ minimumBy (compare `on` tourDistance graph) population
    evolve population generationsRemaining = do
      let sortedPopulation = sortOn (tourDistance graph) population
      let selectedParents = take (populationSize `div` 2) <$> replicateM 2 (selectIndividual sortedPopulation graph)
      children <- concat <$> mapM (uncurry crossover) selectedParents
      mutatedChildren <- mapM mutate (take mutationRate children)
      let newPopulation = take populationSize $ sortedPopulation ++ mutatedChildren
      evolve newPopulation (generationsRemaining - 1)

main :: IO ()
main = do
  let numCities = 5
      populationSize = 50
      numGenerations = 100
      mutationRate = 5
      exampleGraph = [[0, 10, 15, 20, 25],
                      [10, 0, 35, 25, 30],
                      [15, 35, 0, 30, 50],
                      [20, 25, 30, 0, 40],
                      [25, 30, 50, 40, 0]]
  result <- geneticAlgorithm exampleGraph populationSize numGenerations mutationRate
  putStrLn $ "Optimal Tour: " ++ show result
  putStrLn $ "Optimal Distance: " ++ show (tourDistance exampleGraph result)
 
