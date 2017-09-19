--lambda funcions
--  without lambda def
square1 :: Integer -> Integer
square1 x = x^2

--with lambda expression
square2 :: Integer -> Integer
square2 = \x -> x^2

--below is valid lambda abstraction
m1 :: Integer -> Integer -> Integer
m1 = \x -> \y -> x*y

--or following is also valid
m2 :: Integer -> Integer -> Integer
m2 = \x y -> x*y

--solve quadratic equation with lambda function
solveQdr :: (Float,Float,Float) -> (Float,Float)
solveQdr = \(a,b,c) -> if a == 0 then error "Not quadratic"
                      else let d = b^2 - 4*a*c in
                      if d < 0 then error "No real solution"
                      else
                         ((-b + sqrt d)/2*a,
                         (-b - sqrt d)/2*a)
