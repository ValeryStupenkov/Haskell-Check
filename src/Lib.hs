module Lib where

import Data.List
import Data.Time.Calendar
import Data.Time.Clock
import Data.Map

-- Тип для продуктов в каталоге
data Item = Item
  {
  name :: String,
  price :: Double,
  category :: String
  } deriving(Read, Show)

-- Тип для продуктов в корзине
data Position = Position 
  {
  item :: String,
  count :: Int
  } deriving(Read, Show)

-- Тип для бонусной карты
data BonusCard = BonusCard 
  {
  birthDate :: (Integer,Int,Int),
  bonus :: Double
  } deriving(Read, Show)

-- Разделяет строку по указанному символу
splitBy delimiter = Data.List.foldr f [[]] 
  where f c l@(x:xs) | c == delimiter = []:l 
                     | otherwise = (c:x):xs

-- Преобразует разделённую строку во внутренний тип 
toItem :: [String] -> Maybe Item
toItem [n, p, cat] = Just Item {name = n, price = read p, category = cat}
toItem [_] = Nothing

-- Парсит строки в тип Item
toItemList :: [String] -> [Maybe Item]
toItemList catalog = Data.List.map (\x -> toItem $ splitBy ',' x) catalog

-- Преобразует разделённую строку во внутренний тип
toPosition :: [String] -> Maybe Position
toPosition [i, c] = Just Position {item = i, count = read c}
toPosition [_] = Nothing

--
toPositionList :: [String] -> [Maybe Position]
toPositionList cart = Data.List.map (\x -> toPosition $ splitBy ',' x) cart

-- Преобразует разделённую строку во внутренний тип
parseDate :: [String] -> (Integer,Int,Int)
parseDate [y, m, d] = (read y, read m, read d)

-- Преобразует разделённую строку во внутренний тип
toBonusCard :: [String] -> BonusCard
toBonusCard [d, b] = BonusCard { birthDate = parseDate $ splitBy '-' d, bonus = read b}


-- Получает список всех продуктов с количеством и ценами за них
getProductsList :: [Maybe Item] -> [Maybe Position] -> String
getProductsList items (Nothing : xs) = ""
getProductsList items (Just x:xs) =  (item x) ++ " " ++ show (count x) ++ " " ++ show (getPrice items (item x)) ++ " " ++ show (calculatePositionPrice (count x) (getPrice items (item x))) ++ "\n" ++ (getProductsList items xs)


-- Получает цену за продукт в корзине
getPrice :: [Maybe Item] -> String -> Double
getPrice (Nothing : xs) _ = 0
getPrice (Just x : xs) n 
    | name x == n = price x
    | otherwise = getPrice xs n

-- Вычисляет цену за все продукты данного типа
calculatePositionPrice :: Int -> Double -> Double
calculatePositionPrice count price = fromIntegral count * price

-- Возвращает строку общей суммы 
getTotalPrice :: [Maybe Item] -> [Maybe Position] -> String
getTotalPrice catalog cart = "Сумма без скидок  " ++ show (calculateTotalPrice catalog cart) ++ "\n"

-- Вычисляет сумму без скидки
calculateTotalPrice :: [Maybe Item] -> [Maybe Position] -> Double
calculateTotalPrice catalog (Nothing:xs) = 0
calculateTotalPrice catalog (Just x:xs) = calculatePositionPrice (count x) (getPrice catalog (item x)) + calculateTotalPrice catalog xs

-- 
calculateDiscount :: [Maybe Item] -> [Maybe Position] -> BonusCard -> String
calculateDiscount catalog cart bonus = "Суммарная скидка " ++ show ((discountMoreThanFive catalog cart) + (discountCategoryCntTen catalog cart) + (discountBirthday bonus)) ++ "%\n" 

-- 
discountMoreThanFive ::  [Maybe Item] -> [Maybe Position] -> Int
discountMoreThanFive catalog cart 
    | (calculateTotalPrice catalog cart) > 5000 = 20
    | otherwise = 0
   
--
discountCategoryCntTen :: [Maybe Item] -> [Maybe Position] -> Int
discountCategoryCntTen catalog cart
    | maximum(elems (cntCategory catalog cart)) >= 10 = 30
    | otherwise = 0 
    
--
getCategory :: [Maybe Item] -> String -> String
getCategory (Nothing : xs) _ = ""
getCategory (Just x : xs) n 
    | name x == n = category x
    | otherwise = getCategory xs n

--
cntCategory :: [Maybe Item] -> [Maybe Position] -> Map String Int
cntCategory catalog (Nothing:xs) = empty
cntCategory catalog (Just x:xs) = insertWith (+) (getCategory catalog (item x)) (count x) (cntCategory catalog xs)

-- 
discountBirthday :: BonusCard -> Int
discountBirthday bonuscard 
    | isBirthday (birthDate bonuscard) = 50
    | otherwise = 0
    
--
isBirthday :: (Integer, Int, Int) -> Bool
isBirthday (y, m, d) 
    | y == 1969 && m == 8 && d == 15 = True
    | otherwise = False

-- Формирует чек в виде строки
printCheck :: [Maybe Item] -> [Maybe Position] -> BonusCard -> String
printCheck catalog cart bonuscard = (getProductsList catalog cart) ++ (getTotalPrice catalog cart) ++ (calculateDiscount catalog cart bonuscard)
	
	
