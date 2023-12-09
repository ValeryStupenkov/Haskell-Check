module Lib where

import Data.List
import Data.Map

-- Тип для продуктов в каталоге
data Item = Item
  {
  name :: String,
  price :: Double,
  category :: String
  } deriving(Read, Show, Eq)

-- Тип для продуктов в корзине
data Position = Position 
  {
  item :: String,
  count :: Int
  } deriving(Read, Show, Eq)

-- Тип для бонусной карты
data BonusCard = BonusCard 
  {
  birthDate :: (Integer,Int,Int),
  discount :: Double
  } deriving(Read, Show)

-- Разделяет строку по указанному символу
splitBy delimiter = Data.List.foldr f [[]] 
    where f c l@(x:xs) | c == delimiter = []:l 
                     | otherwise = (c:x):xs

-- Проверяет, что строка соддержит числовое значение
isNumeric :: String -> Bool
isNumeric str = case reads str :: [(Double, String)] of
    [(_, "")] -> True
    _-> False

-- Преобразует разделённую строку во внутренний тип 
toItem :: [String] -> Maybe Item
toItem [n, p, cat]
    | isNumeric p = Just Item {name = n, price = read p, category = cat}
    | otherwise = Nothing
toItem _ = Nothing

-- Парсит список строк в каталог
toItemList :: [String] -> [Maybe Item]
toItemList catalog = Data.List.map (\x -> toItem $ splitBy ',' x) catalog

-- Преобразует разделённую строку во внутренний тип
toPosition :: [String] -> Maybe Position
toPosition [i, c] 
    | isNumeric c = Just Position {item = i, count = read c}
    | otherwise = Nothing
toPosition _ = Nothing

-- Преобразует список строк в список продутов
toPositionList :: [String] -> [Maybe Position]
toPositionList cart = Data.List.map (\x -> toPosition $ splitBy ',' x) cart

-- Преобразует разделённую строку во внутренний тип
parseDate :: [String] -> (Integer,Int,Int)
parseDate [y, m, d] = (read y, read m, read d)
parseDate _ =  (0, 0, 0)

-- Преобразует разделённую строку во внутренний тип
toBonusCard :: [String] -> BonusCard
toBonusCard [d, b] = BonusCard { birthDate = parseDate $ splitBy '-' d, discount = read b}


-- Получает список всех продуктов с количеством и ценами за них
getProductsList :: [Maybe Item] -> [Maybe Position] -> String
getProductsList _ (Nothing : _) = ""
getProductsList items (Just x:xs)
    | (getPrice items (item x)) == 0 = "Продукта " ++ (item x) ++ " нет в каталоге!\n" ++ getProductsList items xs
    | otherwise = (item x) ++ " " ++ show (count x) ++ " " ++ show (getPrice items (item x)) ++ " " ++ show (calculatePositionPrice (count x) (getPrice items (item x))) ++ "\n" ++ (getProductsList items xs)
getProductsList _ _ = ""


-- Получает цену за продукт в корзине
getPrice :: [Maybe Item] -> String -> Double
getPrice (Nothing : _) _ = 0
getPrice (Just x : xs) n 
    | name x == n = price x
    | otherwise = getPrice xs n
getPrice _ _ = 0

-- Вычисляет цену за все продукты данного типа
calculatePositionPrice :: Int -> Double -> Double
calculatePositionPrice count price = fromIntegral count * price

-- Возвращает строку общей суммы 
getTotalPrice :: [Maybe Item] -> [Maybe Position] -> String
getTotalPrice catalog cart = "Всего " ++ show (calculateTotalPrice catalog cart) ++ " галактических кредитов" ++ "\n"

-- Вычисляет сумму без скидки
calculateTotalPrice :: [Maybe Item] -> [Maybe Position] -> Double
calculateTotalPrice _ (Nothing:_) = 0
calculateTotalPrice catalog (Just x:xs) = calculatePositionPrice (count x) (getPrice catalog (item x)) + calculateTotalPrice catalog xs
calculateTotalPrice _ _ = 0

-- Вычисляет суммарную скидку
calculateDiscount :: [Maybe Item] -> [Maybe Position] -> BonusCard -> String
calculateDiscount catalog cart bonus = let
    totalDiscount = (discountMoreThanFive catalog cart) + (discountCategoryCntTen catalog cart) + (discountBirthday bonus) + (discount bonus)
    totalPrice = calculateTotalPrice catalog cart
    resultPrice = totalPrice - totalPrice * (totalDiscount / 100)
    in "Суммарная скидка " ++ show totalDiscount ++ "%\n" ++ "Итог " ++ show resultPrice ++ " галактических кредитов\n"

-- Начисляет скидку за корзину на сумму более 5000
discountMoreThanFive ::  [Maybe Item] -> [Maybe Position] -> Double
discountMoreThanFive catalog cart 
    | (calculateTotalPrice catalog cart) > 5000 = 20
    | otherwise = 0
   
-- Начисляет скидку за наличие 10 товаров одной категории
discountCategoryCntTen :: [Maybe Item] -> [Maybe Position] -> Double
discountCategoryCntTen catalog cart
    | maximum(elems (cntCategory catalog cart)) >= 10 = 15
    | otherwise = 0 
    
-- Получает категорию искомого товара
getCategory :: [Maybe Item] -> String -> String
getCategory (Nothing : _) _ = ""
getCategory (Just x : xs) n 
    | name x == n = category x
    | otherwise = getCategory xs n
getCategory _ _ = ""

-- Составляет словарь категорий и количества продуктов в каждой из них
cntCategory :: [Maybe Item] -> [Maybe Position] -> Map String Int
cntCategory _ (Nothing:_) = empty
cntCategory catalog (Just x:xs) = insertWith (+) (getCategory catalog (item x)) (count x) (cntCategory catalog xs)
cntCategory _ _ = empty

-- Начисляет скидку за удачный день рождения
discountBirthday :: BonusCard -> Double
discountBirthday bonuscard 
    | isBirthday (birthDate bonuscard) = 50
    | otherwise = 0
    
-- Проверяет, совпадает ли день рождения с удачной датой
isBirthday :: (Integer, Int, Int) -> Bool
isBirthday (y, m, d) 
    | y == 1969 && m == 8 && d == 15 = True
    | otherwise = False
    
-- Проверяет правильный парсинг каталога и корзины
listIsNotFine :: [Maybe a] -> Bool
listIsNotFine (Just x:xs) = listIsNotFine xs
listIsNotFine [Nothing] = False
listIsNotFine (Nothing:_) = True

-- Проверяет, содержит ли каталог негативные цены
catalogContainsNegative :: [Maybe Item] -> Bool
catalogContainsNegative (Just x:xs)
    | (price x) < 0 = True
    | otherwise = catalogContainsNegative xs
catalogContainsNegative _ = False

-- Проверяет, содержит ли корзина негативные количества
cartContainsNegative :: [Maybe Position] -> Bool
cartContainsNegative (Just x:xs) 
    | (count x) < 0 = True
    | otherwise = cartContainsNegative xs
cartContainsNegative _ = False

-- Формирует чек в виде строки
printCheck :: [Maybe Item] -> [Maybe Position] -> BonusCard -> String
printCheck catalog cart bonuscard
    | (listIsNotFine catalog) || (listIsNotFine cart) = "Ошибка! Неверный формат данных во входных файлах!"
    | (catalogContainsNegative catalog) = "Ошибка! В каталоге есть отрицательные значения!"
    | (cartContainsNegative cart) = "Ошибка! В списке покупок есть отрицательные значения!"
    | (discount bonuscard) < 0 = "Ошибка! Отрицательное значение процентов скидки на бонусной карте!"
    | otherwise = (getProductsList catalog cart) ++ (getTotalPrice catalog cart) ++ (calculateDiscount catalog cart bonuscard)
    
