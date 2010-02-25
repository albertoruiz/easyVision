import EasyVision

f = gaussS 5.7 . float . gray

main = run (camera >>= observe "Gauss" f)
