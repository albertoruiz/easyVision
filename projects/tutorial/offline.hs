import EasyVision
import Data.List(foldl1',tails)

edges = canny (0.05,0.2) . gradients . gaussS 2 . float . grayscale

history k = map (notI . foldl1' orI . reverse . take k) . tail . tails

main = run  $    camera
            >>=  observe "original" rgb
            ~~>  history 5 . map edges
            >>=  observe "edge history" id
            >>=  saveFrame toYUV
