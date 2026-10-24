{-|
Представление дерева через векторы значений, индексов родителей
и левых соседей вдохновлено материалами Aaron W. Hsu:

* "High-performance Tree Wrangling, the APL Way", Dyalog '18:
  <https://www.youtube.com/watch?v=hzPd3umu78g>

* "A Data Parallel Compiler Hosted on the GPU",
  диссертация, Indiana University, 2019:
  <https://hdl.handle.net/2022/24749>

Этот модуль исследует указанные идеи средствами Haskell, ST и vector.
-}

module VectorTree where

import Control.Monad.ST ( ST, runST )
import Control.Monad (forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))

import qualified Data.Vector.Mutable as M
import qualified Data.Vector as V


-- Тип для обычного дерева
data SimpleTree x = SimpleTree x [SimpleTree x]


exampleSimpleTree :: SimpleTree String
exampleSimpleTree =
  SimpleTree "A"
    [ SimpleTree "B" [SimpleTree "D" []]
    , SimpleTree "C" [SimpleTree "E" [], SimpleTree "F" []]
    ]


-- Тип для векторного дерева
data VecTree s x = VecTree
    { parent :: M.MVector s Int
    , left   :: M.MVector s Int
    , values :: M.MVector s x
    }


-- Конструктор дерева, создает корневой узел
makeVecTree :: String -> ST s (VecTree s String)
makeVecTree x = do
    ps <- M.replicate 1 0
    ls <- M.replicate 1 0
    vs <- M.replicate 1 x
    pure (
        VecTree 
            { parent = ps
            , left = ls
            , values = vs })


-- Количество элементов в дереве
vecTreeSize :: (VecTree s String) -> Int
vecTreeSize vec_tree = M.length (values vec_tree)


-- Направление поиска (от начала или от конца)
data VecTreeIterDir = Forward | Reverse


-- Поиск индекса элемента по условию
vecTreeIter :: VecTreeIterDir -> (VecTree s String) -> (Int -> ST s Bool) -> ST s (Maybe Int)
vecTreeIter direction vec_tree condition = 
    let
        tree_size = vecTreeSize vec_tree
        (start, step) = case direction of
            Forward -> (0, 1)
            Reverse -> (tree_size - 1, -1)
        findIndex i
            | i < 0 || tree_size <= i = return Nothing
            | otherwise = do
                found <- condition i
                if found
                    then return (Just i)
                    else findIndex (i + step)
    in findIndex start


-- Модифицировать элементы векторного дерева
vecTreeMap_ :: (String -> String) -> VecTree s String -> ST s ()
vecTreeMap_ f vec_tree = 
    forM_ [0 .. vecTreeSize vec_tree - 1] (\i -> M.modify (values vec_tree) f i)


-- Индекс родителя элемента
getParent :: (VecTree s String) -> String -> ST s (Maybe Int)
getParent tree parentValue = 
    vecTreeIter Forward tree (\index -> do
        value <- M.read (values tree) index
        return (value == parentValue))


getFirstChild :: (VecTree s String) -> Int -> ST s (Maybe Int)
getFirstChild tree targetParent = 
    vecTreeIter Forward tree (\index -> do
        p <- M.read (parent tree) index
        return (index /= targetParent && p == targetParent))


getLastChild :: (VecTree s String) -> Int -> ST s (Maybe Int)
getLastChild tree targetParent = 
    vecTreeIter Reverse tree (\index -> do
        p <- M.read (parent tree) index
        return (index /= targetParent && p == targetParent))


insertVecTree :: VecTree s String -> String -> String -> ST s (Maybe (VecTree s String))
insertVecTree tree target x = runMaybeT $ do
    parent_index <- MaybeT $ getParent tree target

    lift $ do
        last_child_index <- getLastChild tree parent_index

        p <- M.grow (parent tree) 1
        l <- M.grow (left tree) 1
        v <- M.grow (values tree) 1

        let x_index = M.length $ values tree

        M.write p x_index parent_index
        M.write l x_index x_index
        M.write v x_index x

        case last_child_index of
            Nothing -> return ()
            Just lci -> M.write l x_index lci 

        return $ VecTree p l v


snapshot :: VecTree s String -> ST s ([Int], [Int], [String])
snapshot tree = do
    ps <- V.freeze (parent tree)
    ls <- V.freeze (left tree)
    vs <- V.freeze (values tree)
    return (V.toList ps, V.toList ls, V.toList vs)


-- Пример запуска
vecTreeDemoExample :: IO ()
vecTreeDemoExample = do
    let result = runST $ runMaybeT $ do
            treeR <- lift (makeVecTree "R")

            -- tree1 <- MaybeT (insertVecTree treeR "R" "A")
            -- tree2 <- MaybeT (insertVecTree tree1 "R" "B")
            -- tree3 <- MaybeT (insertVecTree tree2 "R" "C")
            -- tree4 <- MaybeT (insertVecTree tree3 "R" "D")
            -- tree5 <- MaybeT (insertVecTree tree4 "R" "E")
            -- tree6 <- MaybeT (insertVecTree tree5 "R" "F")
            -- ([0,0,0,0,0,0,0],[0,1,1,2,3,4,5],["R","A","B","C","D","E","F"])

            -- tree1 <- MaybeT (insertVecTree treeR "R" "A")
            -- tree2 <- MaybeT (insertVecTree tree1 "R" "B")
            -- tree3 <- MaybeT (insertVecTree tree2 "B" "D")
            -- tree4 <- MaybeT (insertVecTree tree3 "R" "E")
            -- tree5 <- MaybeT (insertVecTree tree4 "R" "F")
            -- ([0,0,0,2,0,0],[0,1,1,3,2,4],["R","A","B","D","E","F"])

            tree1 <- MaybeT (insertVecTree treeR "R" "A")
            tree2 <- MaybeT (insertVecTree tree1 "R" "B")
            tree3 <- MaybeT (insertVecTree tree2 "B" "C")
            tree4 <- MaybeT (insertVecTree tree3 "B" "D")
            tree5 <- MaybeT (insertVecTree tree4 "A" "E")
            -- ([0,0,0,2,2,1],[0,1,1,3,3,5],["R","A","B","C","D","E"])

            lift (snapshot tree5)

    print result