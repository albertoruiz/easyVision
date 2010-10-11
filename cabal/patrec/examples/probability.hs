import Util.Probability
import Control.Monad
import Text.Printf


die = uniform ['1'..'6'] :: Prob Char

coin = uniform ['-','+'] :: Prob Char

---------------------------------------------

-- joint dist using explicit monadic/do style
enfer :: Prob (String, String)
enfer = do
    state <- bernoulli (1/1000) "infected" "sane"
    test <- case state of
            "infected" -> bernoulli (99/100) "+" "-"
            "sane" -> bernoulli (95/100) "-" "+"
    return (state,test)

------------------------------------------

-- using joint

urn = bernoulli (10/11) "Good" "Bad"

-- a conditioned distribution on urn
ball "Good" = bernoulli (1/6) "fail" "ok"
ball "Bad"  = bernoulli (1/3) "fail" "ok"

box :: Prob (String, String)
box = joint ball urn

-------------------------------------------

printev prob = printf "%s : %.1f db\n" (show m) evi
    where m = mode prob
          evi = evidence m prob

sep msg x = putStrLn (msg ++ replicate 60 '-') >> print x >> printev x

main = do
    sep "test" enfer
    sep "Jaynes (joint ball urn)" box
    sep "cond on fail" $ ((=="fail").fst) `pfilter` box
    sep "jointWith [] coin die" $ jointWith (\a b->[a,b]) (const coin) die
    sep "using liftM2 (,)" $ liftM2 (,) coin die
    sep "sequence" $ sequence [coin,die]
    sep "jointWith (:) " $ jointWith (:) (const coin) $ jointWith (:) (const coin) (fmap return coin)
    sep "marg snd" $ box `marg` snd
    sep "cond fail" $ box `cond` ((=="fail").fst) `marg` snd
    sep "cond ok" $ pmap snd  $ pfilter ((=="ok").fst) box
