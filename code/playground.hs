doubleMe :: Int -> Int
doubleMe x = x + x

doubleUs :: Int -> Int -> Int
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: Int -> Int
doubleSmallNumber x = if x > 100
                        then x
                        else x*2

doubleSmallNumber' :: Int -> Int
doubleSmallNumber' x = (if x > 100 then x else doubleMe x) + 1

boomBangs :: [Int] -> [String]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' :: Num a => [t] -> a
length' xs = sum [1 | _ <- xs]

removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

name :: Type
 pattern${3/.+/ /}${4/.+/= /}definition
