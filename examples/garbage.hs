-- checking automatic garbage collection

import GSL

m k = constant k (1000000::Int)

-- this is an infinite list of one million-component vectors
x :: [V]
x = map m [0..]

-- this is an infinite list with the corresponding euclidean norms
y :: [Double]
y = map norm x

-- the same list, the intermediate vectors are not kept
z :: [Double]
z = map (norm . m) [0..]

main = do
    print z  -- No problem, but please DO NOT print y
