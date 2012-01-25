import EasyVision

f = gaussS 5.7 . float . grayscale

main = run (camera >>= observe "Gauss" f)
