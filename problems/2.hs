import Numeric.LinearAlgebra

-- Сигмоїдна функція та її похідна
sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

sigmoid' :: Double -> Double
sigmoid' x = x * (1 - x)

-- Функція для прямого проходження через мережу
feedforward :: Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> (Matrix Double, Matrix Double, Matrix Double)
feedforward input weights1 weights2 biases1 biases2 = (hiddenLayerOutput, outputLayerOutput, output)
  where
    hiddenLayerInput = (input <> weights1) + biases1
    hiddenLayerOutput = cmap sigmoid hiddenLayerInput
    outputLayerInput = (hiddenLayerOutput <> weights2) + biases2
    outputLayerOutput = cmap sigmoid outputLayerInput
    output = outputLayerOutput

-- Функція для зворотного поширення помилки та оновлення ваг та зсувів
backpropagation :: Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Double -> (Matrix Double, Matrix Double, Matrix Double, Matrix Double, Matrix Double, Matrix Double)
backpropagation input target weights1 weights2 biases1 biases2 hiddenLayerOutput outputLayerOutput learningRate =
  (newWeights1, newWeights2, newBiases1, newBiases2, hiddenLayerOutput, outputLayerOutput)
  where
    outputError = target - outputLayerOutput
    outputDelta = cmap sigmoid' outputLayerOutput * outputError

    hiddenError = outputDelta <> tr weights2
    hiddenDelta = cmap sigmoid' hiddenLayerOutput * hiddenError

    newWeights2 = weights2 + tr hiddenLayerOutput <> outputDelta * learningRate
    newBiases2 = biases2 + sumElements outputDelta * learningRate

    newWeights1 = weights1 + tr input <> hiddenDelta * learningRate
    newBiases1 = biases1 + sumElements hiddenDelta * learningRate

-- Функція для навчання мережі
train :: Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Double -> Int -> (Matrix Double, Matrix Double, Matrix Double, Matrix Double, Matrix Double, Matrix Double)
train input target weights1 weights2 biases1 biases2 hiddenLayerOutput outputLayerOutput learningRate epochs
  | epochs == 0 = (weights1, weights2, biases1, biases2, hiddenLayerOutput, outputLayerOutput)
  | otherwise =
    train newInput target newWeights1 newWeights2 newBiases1 newBiases2 newHiddenLayerOutput newOutputLayerOutput learningRate (epochs - 1)
  where
    (newHiddenLayerOutput, newOutputLayerOutput, newOutput) = feedforward input weights1 weights2 biases1 biases2
    (newWeights1, newWeights2, newBiases1, newBiases2, _, _) = backpropagation input target weights1 weights2 biases1 biases2 newHiddenLayerOutput newOutputLayerOutput learningRate

-- Приклад використання
main :: IO ()
main = do
  let input = (2><3) [0, 0, 1, 0, 1, 1]  -- Вхідні дані
      target = (2><1) [0, 1]                -- Очікувані вихідні дані
      weights1 = (3><4) [0.4, -0.5, 0.2, -0.1, 0.7, 0.1, 0.6, 0.3, -0.9, -0.4, 0.2, -0.5]  -- Ваги шару 1
      weights2 = (4><1) [0.1, -0.3, 0.7, 0.5]  -- Ваги шару 2
      biases1 = (1><4) [0, 0, 1, 1]            -- Зсуви шару 1
      biases2 = (1><1) [0]                     -- Зсув шару 2
      learningRate = 0.5
      epochs = 10000

  let (finalWeights1, finalWeights2, finalBiases1, finalBiases2, _, finalOutput) = train input target weights1 weights2 biases1 biases2 (fromList [0]) (fromList [0]) learningRate epochs

  putStrLn "Final Output:"
  print finalOutput
