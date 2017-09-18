divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

ld :: Integer -> Integer
ld n = ldf 2 n

ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
        | k^2 > n     = n
        | otherwise   = ldf (k+1) n

prime0 :: Integer -> Bool
prime0 n | n < 1      = error "Not a positive integer"
         | n == 1     = False
         | otherwise  = ld n == n

factors :: Integer -> [Integer]
factors n | n < 1 = error "Only positive integers"
          | n == 1 = []
          | otherwise = p : factors (div n p) where p = ld n

-- ldp for improving the function LD
ldp :: Integer -> Integer
ldp n = ldpf prime1 n

ldpf :: [Integer] -> Integer -> Integer
ldpf (p:ps) n | rem n p == 0 = p
              | p^2 > n = n
              | otherwise = ldpf ps n
prime1 :: [Integer]
prime1 = 2 : filter prime [3..]

prime :: Integer -> Bool
prime n | n < 1 = error "Not positive Integer"
        | n == 1 = False
        | otherwise = ldp n == n
