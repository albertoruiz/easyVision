import Vision.GUI
import Image.Processing
import Image.Devel

main = run $   arr grayf
           >>> observe "keypoints" g

keypoints  = (.>. 0.05) . localMax 1 . salienceC 3

salienceC s1 = ((-1) .*) . hessian . gradients . gaussS s1

g x = Draw [ Draw x, color red $ pointSz 3 (getPoints 300 (keypoints x))]

