module Main (main) where

-- -- Задача с реализацией красночерного дерева
-- import RbTree

-- -- Задача с замерами производительности списка и вектора
-- import ListVectorPerformanceTest

-- Задача с деревьями на базе векторов
import VectorTree

-- -- Задачи с монадическими трансформерами
-- import MonadTransformers

main :: IO ()
main = do
      -- -- Задача с реализацией красночерного дерева
      -- putStrLn "Hello, RB-tree!"
      -- let a = RbTree.insert 5 Empty
      -- putStrLn $ show a
      -- let b = RbTree.insert 2 a
      -- putStrLn $ show b
      -- let c = RbTree.insert 1 b
      -- putStrLn $ show c

      -- -- Задача с замерами производительности списка и вектора
      -- runBenchmark

      -- Задача с деревьями на базе векторов
      vecTreeDemoExample

      -- -- Серия задач на монадические трансформеры
      -- incrementDemoExample1
      -- incrementDemoExample2
      -- playerDemoExample1
      -- playerDemoExample2
