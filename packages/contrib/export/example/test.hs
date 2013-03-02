import HTools

main = do
    k <- hsfun 5
    print k
    print (map hf2 [0..5])

