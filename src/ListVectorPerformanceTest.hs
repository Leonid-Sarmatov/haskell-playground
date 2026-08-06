module ListVectorPerformanceTest where

-- Библиотеки для списка и вектора
import qualified Data.List as L
import qualified Data.Vector.Unboxed as VU

-- Библиотеки для работы с SIMD инструкциями
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.Native as N

-- Библиотека для бэнчмарков
import Test.Tasty.Bench

-- Длинна списка (количество входных данных)
n :: Double
n = 20_000_000

-- ******* конвертация списка в нужную структуру данных *******

-- Создать обычный список из множества чисел
inputListData :: [Double]
inputListData = [1.0 .. n]

-- Превратить список в вектор
listToVec :: [Double] -> VU.Vector Double
listToVec list = VU.fromList list

-- Превратить список в быстрый вектор
listToAccVec :: [Double] -> A.Vector Double
listToAccVec list = A.fromList (A.Z A.:. length list) list

-- ************** пайплайны с обработкой списков **************

-- ОБРАБОТКА ОБЫЧНОГО СПИСКА
-- Использован foldl', чтобы избежать утечки памяти (space leak) на ленивом списке
processList :: [Double] -> Double
processList = foldl' (+) 0 . L.map (+ 1) . L.map (* 2)

-- ОБРАБОТКА ВЕКТОРА
-- Благодаря Stream Fusion, GHC должен соединить map и sum в один цикл без промежуточных массивов
processVec :: VU.Vector Double -> Double
processVec = VU.sum . VU.map (+ 1) . VU.map (* 2)

-- ОБРАБОТКА SIMD-ВЕКТОРА
processAccVec :: A.Acc (A.Vector Double) -> A.Acc (A.Scalar Double)
processAccVec = A.sum . A.map (+ 1) . A.map (* 2)

-- Функция запуска бэнчмарков
runBenchmark :: IO ()
runBenchmark = do
    let inputVecData = listToVec inputListData
    let inputAccVecData = listToAccVec inputListData
    defaultMain
        [ bgroup "Processing 20M Double elements"
            [ bench "Standard List (foldl')"   $ whnf processList inputListData
            , bench "Unboxed Vector (Fusion)"  $ whnf processVec  inputVecData
            , bench "Accelerate Vector (SIMD)" $ whnf N.run $ processAccVec $ A.use inputAccVecData
            ]
        ]

-- Итоговые результаты тестирования скорости:

-- All
--   Processing 20M Double elements
--     Standard List (foldl'):   OK
--       72.4 ms ± 3.7 ms
--     Unboxed Vector (Fusion):  OK
--       27.7 ms ± 2.4 ms
--     Accelerate Vector (SIMD): OK
--       9.48 ms ± 813 μs

-- All 3 tests passed (11.58s)