import EasyVision

edges = canny (0.1,0.3) . gradients . gaussS 2 . float . grayscale

main = run  $    camera
            >>=  observe "Canny's operator" (notI . edges)
