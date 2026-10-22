module MonadTransformers where

import Control.Monad.State.Strict

-- **************** использование State монады ****************

-- Состояние плеера:
--   - воспроизведение записи
--   - запись поставлена на паузу
--   - запись отсутствует
--   - запись загружается в память
data PlayerState = Play | Stop | Empty | Loading deriving (Eq, Show)


-- Пример использования State монады (пример 
-- переключения состояния плеера)
-- 
--                 изменяемое при вычислении состояние
--                 |         результат вычислений
--                 ↓         ↓
play :: State PlayerState String
play = do
    currentState <- get
    case currentState of
        Play -> return "error: player already playing" 
        Stop -> do
            put Play
            return "ok: playing"
        Empty -> return "error: playlist is empty" 
        Loading -> return "warn: player not ready" 


stop :: State PlayerState String
stop = do
    currentState <- get
    case currentState of 
        Play -> do
            put Stop
            return "ok: stopped"
        Stop -> return "error: player already stopped"
        Empty -> return "error: playlist is empty" 
        Loading -> return "warn: player not ready" 


-- Еще пример использования State монады
-- (хранение инкрементируемого счётчика)
increment :: State Int Int
increment = do
    n <- get
    put (n + 1)
    return n


-- Пример запуска (с возвратом
-- промежуточных состояний и с do-нотацией)
--
-- ((10,11,12),13)
incrementDemoExample1 :: IO ()
incrementDemoExample1 = do
    let action = do
            s1 <- increment
            s2 <- increment
            s3 <- increment
            pure (s1, s2, s3)
    print (runState action 10)


-- Пример запуска (без запоминания 
-- промежуточных состояний, через >>)
--
-- ("completed",13)
incrementDemoExample2 :: IO ()
incrementDemoExample2 = do
    let action = increment >> increment >> increment >> pure "completed"
    print (runState action 10)


-- Пример запуска плеера (при повторном запуске - ошибка)
--
-- (("ok: playing","error: player already playing"),Play)
playerDemoExample1 :: IO ()
playerDemoExample1 = do
    let action = do
            s1 <- play
            s2 <- play
            pure (s1, s2)
    print (runState action Stop)


-- Пример запуска плеера (валидные переходы)
--
-- (("ok: playing","ok: stopped"),Stop)
playerDemoExample2 :: IO ()
playerDemoExample2 = do
    let action = do
            s1 <- play
            s2 <- stop
            pure (s1, s2)
    print (runState action Stop)