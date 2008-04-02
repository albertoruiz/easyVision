-- experiments with quaternions

import Vision
import Numeric.LinearAlgebra
import Quaternion

r = rot2 0.2 <> rot3 0.1 <> rot1 0.5

toQuat = uncurry axisToQuat . rotToAxis


main = do
    print r
    print $ getRotation . toQuat $ r
    putStrLn "--------------------"
    print $ toQuat (rot2 0.2) * toQuat (rot3 0.1) * toQuat (rot1 0.5)
    print $ toQuat r
    putStrLn "--------------------"
    print $ getRotation $ slerp (toQuat $ rot2 0.1) (toQuat $ rot2 0.3) 0.5
    print $ rot2 0.2
    --putStrLn "--------------------"
    --print $ slerp (toQuat $ rot3 0.1) (toQuat $ rot2 0.3) 0.75
    --print $ slerp' (toQuat $ rot3 0.1) (toQuat $ rot2 0.3) 0.75
