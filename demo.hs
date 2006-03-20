import GSL
import GSL.Util 

sombrero n = mesh $ reshape n $ realVector l
    where l = [ f x y | x <- ran, y <- ran]
          ran = toList $ linspace n (-2,2)
          f x y = exp (-r2) * cos (2*r2) where r2 = x*x+y*y

main = do 
    sombrero 40
