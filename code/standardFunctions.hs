maximum' :: (ord a) => [a] -> a
maximum' = foldrl (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> a
reverse' = foldl (\acc x -> x : acc)

prodcut' :: (Num a) => [a] -> a
product' = foldrl (*)

filter' :: (a -> Bool) -> [a] -> a
filter' p = foldr (\x acc -> if p x the x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)
