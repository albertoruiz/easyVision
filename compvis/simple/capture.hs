{-# LANGUAGE TemplateHaskell, RecordWildCards, NamedFieldPuns #-}

import EasyVision

autoParam "VParam" "vectorize-"
  [( "sigma","Float" ,realParam 0.6 0 2)
  ,( "thres","Int" ,intParam 140 0 255)
  ,( "scale","Float" ,realParam 1.5 0 5)
  ]

mirror = mirror8u 0 . mirror8u 1

main = run $ camera ~> mirror . grayscale >>= selectROI "jeje" id ~> setROI
           >>= vec .@. winVParam
           >>= observe "img" snd

vec VParam{..} = notI .toGray . autoscale scale. float .notI .toGray
               . gaussS sigma. float . thresholdVal8u (fromIntegral thres) 255 IppCmpGreater

setROI (i,r) = modifyROI (const r) i

autoscale s x = (s*recip mx) .* x
  where (mn,mx) = minmax x

