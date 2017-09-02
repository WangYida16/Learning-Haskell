sieve ::  [Integer] -> [Integer]
sieve (p:xs) = p: sieve [x|x <- xs, x`mod`p /= 0]

primes = sieve [2..]

primeFactors :: Integer -> [(Integer, Integer)]
primeFactors 1 = []
primeFactors n = (times, number):primeFactors rest
        where number = head [x|x <- primes, n`mod`x == 0]
              times = time n number
              rest = n `div` (number^times)

time :: Integer -> Integer -> Integer
time n t = if n`mod`t == 0 then 1 + time (n `div` t) t else 0