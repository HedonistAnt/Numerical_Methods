
f = \x -> x^2 - tan (0.5 * x + 0.2)
f' = \ x -> 2*x - 0.5 / ((cos(0.2 + 0.5*x))^2)


stop::Float -> Float -> Float ->  Bool
stop x xp eps = if sqrt ((x - xp)^2) < eps then True else False

newx :: (Float -> Float) -> (Float -> Float) -> Float -> Float
newx f f' x = x - (f x )/(f' x)



newton x xp eps k = if k>100 then (0,0,"Error") else if stop x xp eps == True then (x,k,"OK") else newton (newx f f' x) x eps (k+1)
newton' x = newton x  (x-1) 0.001 0

