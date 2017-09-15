mnmInt :: [Int] -> Int
mnmInt [] = error "Empty List"
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

listMax :: [Int] -> Int
listMax [] = error "List is empty"
listMax [x] = x
listMax (x:xs) = max x (listMax xs)

removeFirstOccurence :: [Int] -> [Int]
removeFirstOccurence _ [] = []
removeFirstOccurence x (y:ys) | x == y = ys
                              | otherwise = y : removeFirstOccurence x ys

sortsInts :: [Int] -> [Int]
sortsInts [] = []
sortsInts xs = z : (sortsInts(removeFirstOccurence z xs)) where z = mnmInt xs
