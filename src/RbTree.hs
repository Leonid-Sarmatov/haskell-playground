module RbTree where

-- Тип ключа
type RbKey = Int

-- Тип цвета (лмбо черный либо красный)
data RbColour = B | R deriving (Show)

-- Тип для дерева
--               вариант если у дерева (узла) есть поддеревья (дочерние узлы)
--               |
--               |                                    вариант если перед нами лист
--               ↓                                    ↓
data RbTree = RbTree RbColour RbTree RbKey RbTree | Empty deriving (Show)
--                       ↑       ↑      ↑     ↑
--                       |       |      |     правое поддерево (или лист)
--                       |       |      ключ 
--                       |       левое поддерево (или лист)  
--                       цвет узла                              

-- вставка нового узла (корень всегда должен быть черный)
--          вставляемый ключ
--          |        дерево для вставки узла
--          |        |         результат работы
--          ↓        ↓         ↓
insert :: RbKey -> RbTree -> RbTree
insert x tree = setBlack $ insRec x tree

-- перекрашивание в черный (для перекрашивания корня при перебалансировки)
setBlack :: RbTree -> RbTree
setBlack (RbTree _ l k r) = (RbTree B l k r)      -- определение для дерева (RbTree)
setBlack Empty = Empty                            -- определение для листа  (Empty)

-- рекурсивный обход дерева для вставки нового узла в дерево
insRec :: RbKey -> RbTree -> RbTree
insRec x Empty = RbTree R Empty x Empty           -- определение для листа (вернуть узел с листами)
insRec x (RbTree c l k r)                         -- определение для дерева (рекурсивный спуск до листа)
    | x < k     = rebalance c (insRec x l) k r    -- спуск влево
    | otherwise = rebalance c l k (insRec x r)    -- спуск влево

-- перебалансировка дерева
rebalance :: RbColour -> RbTree -> RbKey -> RbTree -> RbTree
rebalance B (RbTree R (RbTree R a x b) y c) z d = RbTree R (RbTree B a x b) y (RbTree B c z d)
rebalance B (RbTree R a x (RbTree R b y c)) z d = RbTree R (RbTree B a x b) y (RbTree B c z d)
rebalance B a x (RbTree R (RbTree R b y c) z d) = RbTree R (RbTree B a x b) y (RbTree B c z d)
rebalance B a x (RbTree R b y (RbTree R c z d)) = RbTree R (RbTree B a x b) y (RbTree B c z d)
rebalance c l k r = RbTree c l k r