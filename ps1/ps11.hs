import Control.Parallel.Strategies (parMap, rpar)

-- Simpson's Rule (A)
simpsonsRule :: (Double -> Double) -> Double -> Double -> Double
simpsonsRule f a b =
    let h = (b - a) / 2
        m = (a + b) / 2
    in (h / 3) * (f a + 4 * f m + f b)

-- Adaptive Simpson's Rule (Parallel, B)
adaptiveStrategy :: (Double -> Double) -> Double -> Double -> Double -> Double
adaptiveStrategy f a b sigma =
    let oldApprox = simpsonsRule f a b
        m = (a + b) / 2
        leftApprox = simpsonsRule f a m
        rightApprox = simpsonsRule f m b
        newApprox = leftApprox + rightApprox
    in if abs (newApprox - oldApprox) < 15 * sigma
        then newApprox  -- Stop refining
        else let [left, right] = parMap rpar id [adaptiveStrategy f a m (sigma / 2), adaptiveStrategy f m b (sigma / 2)]
             in left + right  -- Recurse in parallel

-- Ex functions
square :: Double -> Double
square x = x ** 2

cube :: Double -> Double
cube x = x ** 3

main :: IO ()
main = do
    let a = 0
        b = 1
        sigma = 1e-6
    putStrLn "Adaptive Integration of x^2 from 0 to 1:"
    print $ adaptiveStrategy square a b sigma
    putStrLn "Adaptive Integration of x^3 from 0 to 1:"
    print $ adaptiveStrategy cube a b sigma


-- C
{-

Simpson’s Rule O(1) is fast but less accurate for non-linear functions. 
While Adaptive Simpson’s Rule O(n) improves accuracy by recursively refining intervals. 
The Adaptive method benefits from parallelism but addes overhead, especially for simple functions. 
Execution time depends on function complexity, tolerance (σ), and recursion depth.

-}