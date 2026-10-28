{-|
Решение задачи Score of a String, ссылка на оригинал:
    https://leetcode.com/problems/score-of-a-string/description/

Оригинальное описание задачи:
    You are given a string s. The score of a string is defined as the 
    sum of the absolute difference between the ASCII values of adjacent characters.
    Return the score of s.
-}

module AsciiSum where

import Data.Char (ord)
import Data.Function


-- Простой вариант разницы между ASCII символами
-- asciiDiff :: Char -> Char -> Int
-- asciiDiff x y = abs ((ord x) - (ord y))


-- Вариант функции разницы между ASCII через комбинаторы
asciiDiff :: Char -> Char -> Int
asciiDiff = (abs .) . ((-) `on` ord)


{-|
Развёртка сигнатур функций при использовании комбинаторов

    f :: (Char -> Int) -> Char -> Int
    f = (abs .)

    g :: Char -> (Char -> Int)
    g = ((-) `on` ord)

    asciiDiff :: Char -> Char -> Int
    asciiDiff = f . g
-}


-- Вариант решения через let-in
asciiSum :: String -> Int
asciiSum [] = 0
asciiSum (x:(y:[])) = asciiDiff x y
asciiSum (x:xs) = 
    let
        y = head xs
    in
       asciiDiff x y + asciiSum xs


-- Вариант решения через case-of и where
asciiSum2 :: String -> Int
asciiSum2 l = case l of 
        (x:(y:[])) -> diff x y
        (x:(y:ys)) -> diff x y + asciiSum2 ys
        _ -> 0
    where
        diff = (abs .) . ((-) `on` ord)


-- Пример запуска
asciiSumDemoExample :: IO ()
asciiSumDemoExample = do
    print $ asciiSum "hello"
    -- 13
    print $ asciiSum2 "zaz"
    -- 50