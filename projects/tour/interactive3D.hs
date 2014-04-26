import Vision.GUI.Simple
import Util.Geometry

pause = putStrLn "Press any key (here) to continue..." >> getChar

-- the following session can also be done in ghci

main = do
    (reset,add) <- interactive3D "my drawing"
    pause
    reset $ axes3D 4
    pause
    let r = [1 .. 3]
    reset $ Draw [axes3D 4, Draw [Point3D x y z | x <- r, y <- r, z <- r ]]
    pause
    reset $ clearColor white [color black (axes3D 4)
                              , (color red . pointSz 3) [Point3D x y z | x <- r, y <- r, z <- r ]]
    pause
    let r = [1,1.5 .. 3]
    let ps = [Point3D x y z | x <- r, y <- r, z <- r ]
    reset $ Draw [color black (axes3D 4) , (color red . pointSz 3) ps]
    pause
    reset $ clearColor white (color black (axes3D 5))
    pause
    let p1 = Point3D 2 1 1
        p2 = Point3D 3 3 3
    add $ (color green . pointSz 5) [p1, p2]
    pause
    let l12 = gjoin p1 p2
    add $ color gray l12
    pause
    
