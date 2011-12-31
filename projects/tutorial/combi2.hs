import EasyVision

drift r a b = r .* a |+| (1-r) .* b

main = run  $    camera
            ~>   float . grayscale
            ~~>  scanl1 (drift 0.9)
            >>=  observe "drift" id

