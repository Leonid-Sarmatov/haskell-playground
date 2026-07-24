module Main (main) where

import qualified RbTree
import RbTree (RbTree(Empty))

main :: IO ()
main = do
  putStrLn "Hello, RB-tree!"
  let a = RbTree.insert 5 Empty
  putStrLn $ show a
  let b = RbTree.insert 2 a
  putStrLn $ show b
  let c = RbTree.insert 1 b
  putStrLn $ show c


