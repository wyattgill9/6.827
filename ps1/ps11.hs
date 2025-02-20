-- Problem 1
-- A

compositeStrategy :: (Double -> Double) -> Double -> Double -> Int -> Double
compositeStrategy f a b n =
    let h = (b - a) / fromIntegral (2 * n)
        x i = a + fromIntegral i * h
        coefficient i
            | i == 0 || i == 2 * n = 1
            | even i = 2
            | otherwise = 4
        terms = [coefficient i * f (x i) | i <- [0 .. 2 * n]]
    in (h / 3) * sum terms

-- Ex functions to test
square :: Double -> Double
square x = x ** 2

cube :: Double -> Double
cube x = x ** 3

main :: IO ()
main = do
    let a = 0
        b = 1
        n = 10
    putStrLn "Integrating x^2 from 0-1:"
    print $ compositeStrategy square a b n
    putStrLn "Integrating x^3 from 0-1:"
    print $ compositeStrategy cube a b n

-- B