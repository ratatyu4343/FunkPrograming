type Position = (Int, Int)

-- Функція, яка знаходить шлях коня на шахівниці
knightTour :: Int -> [Position]
knightTour n = knightTour' n (1, 1) []

-- Допоміжна функція для рекурсивного знаходження шляху
knightTour' :: Int -> Position -> [Position] -> [Position]
knightTour' n currentPath@(x, y) visited
  | length visited == n * n = reverse visited
  | otherwise =
    let possibleMoves = filter (\pos -> pos `notElem` visited && isValidMove pos) (nextMoves currentPath)
     in case possibleMoves of
          [] -> [] -- Якщо немає можливих ходів, зупиняємося
          _ ->
            case foldr (\pos acc -> acc || knightTour' n pos (pos : visited) /= []) False possibleMoves of
              True -> reverse visited
              False -> []

-- Функція, яка перевіряє, чи дозволений хід на шахівниці
isValidMove :: Position -> Bool
isValidMove (x, y) = x > 0 && y > 0 && x <= 8 && y <= 8

-- Функція, яка генерує всі можливі ходи коня
nextMoves :: Position -> [Position]
nextMoves (x, y) =
  filter isValidMove
    [ (x + 2, y + 1),
      (x + 2, y - 1),
      (x - 2, y + 1),
      (x - 2, y - 1),
      (x + 1, y + 2),
      (x + 1, y - 2),
      (x - 1, y + 2),
      (x - 1, y - 2)
    ]

main :: IO ()
main = print $ knightTour 8
