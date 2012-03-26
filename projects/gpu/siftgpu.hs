-- demo of siftgpu

import Vision.GUI
import ImagProc
import ImagProc.GPU.SIFT

main = do
    prepare
    runNT_ camera $ sift grayscale >>> observe "SIFT GPU" sh

sh (x, feats) = Draw [ Draw (rgb x), color yellow feats ]

