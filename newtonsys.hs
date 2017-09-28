import Data.Matrix as M 
f1 x y = 1-sin(y+0.5)+x
f2 x y = -y-cos(x-2)
df1x x = 1
df1y y = -cos(y+0.5)
df2x x = -sin(2-x)
df2y y = (-1)

j x y = M.fromLists [[df1x x, df1y y],[df2x x, df2y y]]
ij x y = M.inverse (j x y)
f x y = M.fromLists [[f1 x y],[f2 x y]]


stop x xp y yp eps = if  sqrt((x-xp)^2 + (y-yp)^2) < eps then True else False

newVmatrix x y = M.fromLists[[x],[y]] - (M.multStd (j x y) (f x y)) 
newV x y = M.toList(newVmatrix x y)


newtonsys x xp y yp eps k = if stop x xp y yp eps == True then (x,y,k) else newtonsys ((newV x y)!!0) x ((newV x y)!!1) y eps (k+1)

newtonsys' x y = newtonsys x (x-1) y (y-1) 0.001 0
