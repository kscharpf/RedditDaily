{-# LANGUAGE DeriveDataTypeable #-}

import System.Random
import Data.Char
import System.Console.CmdArgs

getNextChar :: (RandomGen g) => g -> (Char, g)
getNextChar g = randomR ('A', 'z') g

getPassword :: (RandomGen g) => g -> Int -> [Char] -> [Char]
getPassword g 0 ys = ys
getPassword g n ys = getPassword g2 (n-1) (ys ++ [nextC])
                         where (nextC, g2) = getNextChar g

data Args = Args {passlen :: Int} deriving (Data, Typeable, Show)

main = do
  x <- cmdArgs $ Args 7
  g <- newStdGen
  print (getPassword g (passlen x) [])

