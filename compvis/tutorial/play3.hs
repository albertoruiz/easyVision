import EasyVision

main = run  $    camera
            >>=  observe "original" rgb
            ~>   highPass8u Mask5x5 . median Mask5x5 . grayscale
            >>=  observe "high-pass filter" id
