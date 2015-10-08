module Main where

import Data.Complex
import Data.List
import System.Environment (getArgs)

width = 170
height = 45

black = "\x1b[30m"
red = "\x1b[31m"
green = "\x1b[32m"
yellow = "\x1b[33m"
blue = "\x1b[34m"
magenta = "\x1b[35m"
cyan = "\x1b[36m"
white = "\x1b[37m"

blackb = "\x1b[30;1m"
redb = "\x1b[31;1m"
greenb = "\x1b[32;1m"
yellowb = "\x1b[33;1m"
blueb = "\x1b[34;1m"
magentab = "\x1b[35;1m"
cyanb = "\x1b[36;1m"
whiteb = "\x1b[37;1m"

reset = "\x1b[0m"

toString :: Int -> String
toString 1000 = black ++ "#" ++ reset
toString 1 = red ++ "0" ++ reset
toString 2 = yellow ++ "0" ++ reset
toString 3 = cyan ++ "0" ++ reset
toString 4 = magenta ++ "0" ++ reset
toString 5 = green ++ "0" ++ reset
toString n
    | mod n 6 == 0 = greenb ++ "0" ++ reset
    | mod n 5 == 0 = magentab ++ "0" ++ reset
    | mod n 4 == 0 = cyanb ++ "0" ++ reset
    | mod n 3 == 0 = yellowb ++ "0" ++ reset
    | mod n 2 == 0 = redb ++ "0" ++ reset
toString _ = green ++ "-" ++ reset


mandelbrot :: Complex Double -> [Complex Double]
mandelbrot c = iterate (\z -> (z ^ 2) + c) (0 :+ 0)

timeToDiverge :: Complex Double -> Int
timeToDiverge = length . takeWhile (\c -> magnitude c < 2) . take 1000 . mandelbrot

--toMandleSpace :: Complex Double -> Complex Double
-- x0 = -2.5
-- x1 = 1
-- y0 = -1
-- y1 = 1
toMandleSpace (x0, y0) (x1, y1) point = do
    let pr = realPart point
        pi = imagPart point
        x = (pr / width) * (x1 - x0) + x0
        y = (pi / height) * (y1 - y0) + y0
    x :+ y

calculate :: (Double, Double) -> (Double, Double) -> [[Complex Double]] -> [[Int]]
calculate bottomLeft topRight = fmap $ fmap $ timeToDiverge . (toMandleSpace bottomLeft topRight)

rowToString :: [Int] -> String
rowToString a = concat $ fmap toString a

setToString :: [[Int]] -> String
setToString a = intercalate "\n" $ fmap rowToString a

main :: IO ()
main = do
    args <- getArgs
    if length args == 0
    then _default
    else nonDefault args

_default :: IO ()
_default = do
    let screenArray = [[x :+ y | x <- [0..(width-1)]] | y <- [0..(height-1)]]
        valuearray = calculate (-2.5, (-1)) (1, 1) screenArray
    putStrLn $ setToString valuearray

nonDefault :: [String] -> IO ()
nonDefault args = do
    let x0 = (read $ args !! 0 :: Double)
        y0 = (read $ args !! 1 :: Double)
        x1 = (read $ args !! 2 :: Double)
        y1 = (read $ args !! 3 :: Double)
        screenArray = [[x :+ y | x <- [0..(width-1)]] | y <- [0..(height-1)]]
        valuearray = calculate (x0, y0) (x1, y1) screenArray
    putStrLn $ setToString valuearray
