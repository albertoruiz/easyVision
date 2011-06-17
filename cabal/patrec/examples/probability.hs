{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

import Util.Probability
import Control.Monad
import Text.Printf
import Control.Applicative((<$>),(<*>),pure)

die = uniform [1..6] :: Prob Int

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

instance (Ord a, Eq (Prob a), Num a) => Num (Prob a) where
    fromInteger = return.fromInteger
    (+) a b = (+) <$> a <*> b
    (-) a b = (-) <$> a <*> b
    (*) a b = (*) <$> a <*> b
    abs = fmap abs
    signum = fmap signum



multidice = do
    d0 <- die
    s <- replicateM d0 die
    return (sum s)

-------------------------------------------

printev prob = printf "%s : %.1f db\n" (show m) evi
    where m = mode prob
          evi = evidence m prob

sep msg x = putStrLn (msg ++ replicate 60 '-') >> print x >> printev x

main = do
    sep "test" enfer
    sep "Jaynes (joint ball urn)" box
    sep "cond on fail" $ ((=="fail").fst) `pfilter` box
    sep "jointWith [] coin die" $ jointWith (\a b->[a, (head . show) b]) (const coin) die
    sep "using liftM2 (,)" $ liftM2 (,) coin die
    sep "sequence" $ sequence [coin, (head.show) <$> die] -- need same type in the list
    sep "jointWith (:) " $ jointWith (:) (const coin) $ jointWith (:) (const coin) (fmap return coin)
    sep "marg snd" $ box `marg` snd
    sep "cond fail" $ box `cond` ((=="fail").fst) `marg` snd
    sep "cond ok" $ pmap snd  $ pfilter ((=="ok").fst) box
    sep "exper1" $ exper1 `cond` ((>=10).fst) `marg` (head.snd)
    sep "exper2" $ exper2 `cond` ((==6).fst)
    sep "joint" $ joint (return .length.filter (=='+')) (replicateM 5 coin)
    sep "marg" $ replicateM 5 coin `marg` (length.filter (=='+'))
----------------------------------------------------------------------

exper1 = joint (return.sum) (replicateM 3 die)

exper2 = joint (\n-> sum <$> (replicateM n die)) die


