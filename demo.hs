import GSL
import GSL.Instances 

sombrero n = mesh (f x y) where 
    (x,y) = meshdom range range
    range = linspace n (-2,2)
    f x y = exp (-r2) * cos (2*r2) where 
        r2 = x*x+y*y

main = do 
    sombrero 40
