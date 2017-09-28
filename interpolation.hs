
f = \x -> tan (0.5 * x + 0.2) - x^2
unitX = [(-3), (-2.5), (-2), (-1.5), (-1), (-0.5), 0, 0.5, 1, 1.5, 2]
unitY = map f unitX
pX=1.3
dMul :: Double -> Double -> Int -> Int -> [Double] -> Double

dMul r x i j unitlist 
 | i < (length unitlist) = if i/=j then dMul (r*(x-(unitlist!!i))) x (i+1) j unitlist else (dMul r x (i+1) j unitlist)
 | otherwise = r

l_ :: [Double] -> Int -> [Double]

l_ unitlist i
 | i<(length unitX) = l_ (unitlist ++ [(dMul 1 (unitX!!i) 0 i unitX)]) (i+1) 
 | otherwise = unitlist

l :: [Double] -> Int -> [Double]

l unitlist i
 | i<(length unitX) = l (unitlist ++ [(dMul 1 pX 0 i unitX)]) (i+1) 
 | otherwise = unitlist

interpolate = sum (zipWith (*) (zipWith (/) (l [] 0) (l_ [] 0)) unitY)


