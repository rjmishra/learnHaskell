mnmInt :: [Int] -> Int
mnmInt [] = error "Empty List"
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

listMax :: [Int] -> Int
listMax [] = error "List is empty"
listMax [x] = x
listMax (x:xs) = max x (listMax xs)


removeFirstOccurence _ [] = []
removeFirstOccurence x (y:ys) | x == y = ys
                              | otherwise = y : removeFirstOccurence x ys

sortsInts :: [Int] -> [Int]
sortsInts [] = []
sortsInts xs = z : (sortsInts(removeFirstOccurence z xs)) where z = mnmInt xs

average :: [Int] -> Float
average [] = error "Empty list"
average xs = fromIntegral(sum xs) / fromIntegral (length xs)

sumA :: [Int] -> Int
sumA [] = 0
sumA (x:xs) = x + sumA xs

lengthA :: [a] -> Int
lengthA [] = 0
lengthA (x:xs) = 1 + lengthA xs

-- function to count the number of occurence of a character in given string
count :: Char -> String -> Int
count _ [] = 0
count a (x:xs) | a == x = 1 + count a xs
               | otherwise = count a xs

-- exercise 1.14 Blowup String

--blowHelp :: Int -> String -> String
blowHelp _ [] = []
blowHelp a (x:xs) = replicate (a - length xs) x ++ blowHelp a xs

blowUp [] = []
blowUp (x:xs) = blowHelp (length(x:xs)) (x:xs)


-- function to sort string in alphabetic order
sortString :: String -> String
sortString [] = []
sortString (x:xs) = sortString l ++ midString ++ sortString r
                    where l = [y | y <- xs, y < x]
                          midString = [y| y <-xs , y==x] ++ [x]
                          r = [y | y<-xs, y > x]

-- prefix of a string
prefix :: String -> String -> Bool
prefix [] (x:xs) = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys

-- substring
-- if xs is prefix of ys, xs is substring of ys
-- if ys equals y:ys' and xs is substring of ys' then xs is substring of ys
-- nothing else is substring of ys

substring :: String -> String -> Bool
substring [] (x:xs) = True
substring (x:xs) [] = False
substring xs (y:ys) | prefix (xs) (y:ys) = True
                    | substring (xs) (ys) = True
                    | otherwise = False

-- haskell types Int, Integer, Float, Double, Bool, Char, String
