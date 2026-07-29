module UpsertTask where

-- import System.Random (randomIO)

-- Тип для Id
type Id = Int

-- Тип для полезной нагрузки
type Content = String

-- Тип для статуса вставки
--   Nil      - пустой (зарезервированный) статус 
--   Inserted - элемент успешно добавлен в кэш (но он может быть 
--              вытеснен последующими вставками)
--   Evictum  - элемент был когда то добавлен,
--              но эго вытеснили из кэша
--   Oversize - элемент не может быть вставлен в кэш из за размера
data Status = Nil | Inserted | Evictum | Oversize deriving (Show)

-- Тип для вставляемого элемента
data Item = Item Id Content deriving (Show)

-- Тип для результата вставки элемента
data Result = Result Id Status deriving (Show)

-- Вставить элемент в кэш (внутри какая то реализация, которая не важна)
upsert :: Id -> Content -> Bool
upsert _ _ = True              -- True или False с вероятностью 50/50

-- Проверить, есть ли элемент в кэше (внутри какая то реализация, которая не важна)
isExists :: Id -> Bool
isExists _ = False             -- True или False с вероятностью 50/50

-- *************** решение задачи через таблицы ***************

-- Таблица переходов
resolveActionsTable :: Bool -> Id -> Result
resolveActionsTable False i = Result i Oversize
resolveActionsTable True  i = Result i Inserted

-- Таблица переходов
isExistsActionsTable :: Bool -> Id -> Result
isExistsActionsTable False i = Result i Evictum
isExistsActionsTable True  i = Result i Inserted

-- Вызвать проверку на вытеснение, только если была успешная вставка
isExistsExecute :: Result -> Result
isExistsExecute (Result i Inserted) = isExistsActionsTable (isExists i) i 
isExistsExecute x                   = x

-- Выводить нужно только определенных состояний
printActionsTable :: Result -> IO ()
printActionsTable (Result _ Nil)      = return ()
printActionsTable (Result _ Inserted) = return () 
printActionsTable (Result i Evictum)  = putStrLn $ show i ++ "evictum"
printActionsTable (Result i Oversize) = putStrLn $ show i ++ "oversize"

resolveAll :: [Item] -> [Result]
resolveAll items = 
    -- после вставки проверить на вытеснение
    (\r -> isExistsExecute r)
    <$> 
    -- вставка данных в кэш
    (\(Item i c) -> resolveActionsTable (upsert i c) i)
    <$> 
    -- исходные данные
    items

printAll :: [Result] -> [IO ()]
printAll results = 
    -- печать данных
    (\res -> printActionsTable res)
    <$>
    -- исходные данные
    results

-- ************ решение задачи обычным ветвлением *************

resolveAll2 :: [Item] -> [Result]
resolveAll2 items = 
    -- после вставки проверить на вытеснение
    (\r -> case r of
        Result i Inserted -> if isExists i then Result i Inserted else Result i Evictum
        _                 -> r) 
    <$>
    -- вставка данных в кэш
    (\(Item i c) -> if upsert i c then Result i Inserted else Result i Oversize) 
    <$>
    -- исходные данные
    items 

printAll2 :: [Result] -> [IO ()]
printAll2 results = 
    -- печать данных
    (\res -> case res of
        Result _ Nil      -> return ()
        Result _ Inserted -> return ()
        Result i Evictum  -> putStrLn $ show i ++ "evictum"
        Result i Oversize -> putStrLn $ show i ++ "oversize") 
    <$> 
    -- исходные данные
    results