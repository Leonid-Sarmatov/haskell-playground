module Main (main) where

-- -- Задача с реализацией красночерного дерева
-- import RbTree

-- Задача с замерами производительности списка и вектора
import ListVectorPerformanceTest

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

  -- Задача с замерами производительности списка и вектора
  runBenchmark


