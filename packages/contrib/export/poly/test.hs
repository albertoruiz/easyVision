-- ghc test.hs -o testh

import HTools

import Contours

c = fst (last (pentominos))

main = do
    print c
    print (fun c)

