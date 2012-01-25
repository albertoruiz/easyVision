import EasyVision

edges = notI . canny (0.05,0.2) . gradients . gaussS 2 . float . grayscale

main = process (map edges)
