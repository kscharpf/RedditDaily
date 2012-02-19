import Data.Ratio

fact :: Integer -> Integer
fact 0 = 1
fact n = n * (fact (n-1))

pi_term_numerator :: Integer -> Integer
pi_term_numerator k = (fact (4 * k)) * (1103 + (26390 * k))

pi_term_denominator :: Integer -> Integer
pi_term_denominator k = ((fact k) ^ 4) * (396 ^ (4 * k))

pi_terms :: Integer -> Rational
pi_terms n 
  | n < 0 = 0
  | otherwise = (pi_term_denominator n) % (pi_term_numerator n) + (pi_terms (n-1)) 

-- using Srinivasa Ramanujan's algorithm
--my_pi :: Fractional b => Integer -> b
--my_pi n = (9801 % (sqrt 8)) * (pi_terms n)

--main :: IO ()
--main = do
  --putStrLn (show (my_pi 6))

