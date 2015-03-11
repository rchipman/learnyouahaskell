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
factorial 0 = 1
factorial n = n * factorial (n - 1)

circumference :: Float -> Float
circumference r = 2 * pi * r

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky Number Seven!"
lucky x = "Sorry, you're out of luck pal"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "Can't call head' on an empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long, here are the first two elements: " ++ show x ++ " and " ++ show y

lengthRecursive :: (Num b) => [a] -> b
lengthRecursive [] = 0
lengthRecursive (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

capital :: String -> String
capital "" = "Empty String, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmi :: Float -> Float -> Float
bmi x y = (x / (y * y)) * 703

bmiTell :: Float -> Float -> String
bmiTell weight height
    | bmi <= skinny = "You are underweight, eat some food."
    | bmi <= normal = "You are normal and healthy, nice work."
    | bmi <= fat = "You are bit overweight, might want to switch to salads."
    | otherwise = "You are morbidly obese, stop drinking sugary sodas and go for a 45 min walk each day."
    where bmi = (weight / height ^ 2) * 703
          (skinny, normal,fat) = (18.5, 25.0, 30.0)

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = (w / h ^ 2) *  703]


cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

describeList :: [a] -> String
describeList xs = "The list is " ++ typeOf xs
    where typeOf [] = "empty."
          typeOf [a] = "a singleton list."
          typeOf xs = "a longer list."

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "You cannot get the maximum value of an empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter' (<=x) xs)
        biggerSorted = quicksort (filter' (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd n  = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) $ scanl1 (+) (map sqrt [1..])) + 1

oddSquareSum :: Integer
-- | oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
-- | oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2)
oddSquareSum =
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in sum belowLimit
