module Util.Experiment(
    experiment,
    try, 
    tryS,
    select,
    delete,
    Exper,
    export,
    exportPDF) where

import System.Random
import Control.Arrow
import Data.List(intercalate)
import Numeric.HMatrix
import Util.Text
import System.Exit(ExitCode(..))


type Trials = Int
type Result = Double

-- random experiment, depending on a seed to generate random outcomes
type Exper = Seed -> Result

type Name = String

type Case = [(Name,(String,String))] -- (name,value)

type Expers = [(Case,Exper)]
type Results = [(Case,[Result])]


replic :: Seed -> Trials -> Exper -> [Double]
replic seed n fun = map fun seeds
  where
    seeds = take n (randoms gen)
    gen = mkStdGen seed


replics :: Seed -> Trials -> Expers -> Results
-- same seed?
replics seed n funs = map (reverse *** replic seed n) funs


try :: Show v => Name -> [v] -> [(Case, v -> f)] -> [(Case,f)]
try name vals = concatMap g
  where
    h (s,f) v = ((name, (show v,"")):s, f v)
    g af = map (h af) vals 

tryS :: Name -> [(v,String)] -> [(Case, v -> f)] -> [(Case,f)]
tryS name vals = concatMap g
  where
    h (s,f) (v,sym) = ((name, ("",sym)):s, f v)
    g af = map (h af) vals

experiment :: Seed -> Trials -> t -> ([([a], t)] -> Expers) -> Results
experiment seed n f t = replics seed n (t [([],f)])

select :: (Eq t, Show a)
       => (t, a) -> [([(t, String)], b)] -> [([(t, String)], b)]
select (n,v) = filter (any (== (n, show v)) . fst)

delete :: (Eq t, Show a)
       => (t, a) -> [([(t, String)], b)] -> [([(t, String)], b)]
delete (n,v) = filter (not . any (== (n, show v)) . fst)

{-
export :: FilePath -> Results -> [[Char]]-> IO ()
export name rs codes = do
    let m = fromColumns (map (fromList.snd) rs)
        sm = dropWhile (/='\n') . dispf 5 $ m
        f (n,(v,"")) = n++"="++v
        f (_,(_,s))  = s
        quote s = "\""++s++"\""
        s | null codes = concat . intersperse ";" $ map (quote . unwords . map f. fst) rs
          | otherwise  = concat . intersperse ";" $ map quote $ take (cols m) $ sequence codes
        
    writeFile (name++"dat.txt") sm
    writeFile (name++".m") . unlines $
        [ "load(\""++name++"dat.txt\");"
        , "boxplot("++name++"dat);"
        , "tics(\"x\", 1:" ++ show (cols m)++",{" ++ s ++ "});"
        , "axis([0,"++show (cols m+1)++",0.9,2]);"
        , "hold; plot([0,"++show (cols m+1)++"],[1,1],'g');"
        , "plot([0,"++show (cols m+1)++"],[0,0],'b'); hold;"
--        , "print(\""++name++".svg\");"
--        , "exit"
        ]
    -- system ("octave -q "++name++".m") -- fix this!
    return ()

-- add parameter for axis range and green line at height 1
-- sequence better column codes

-}

--------------------------------------------------------------------------------

export :: String -> FilePath -> Results -> [String] -> IO ()
export sep name rs epilog = do
    let m = fromColumns (map (fromList.snd) rs)
        sm = dropWhile (/='\n') . dispf 5 $ m
        f (n,(v,"")) = n++"="++v
        f (_,(_,a))  = a
        quote a = "\""++a++"\""
        s = intercalate ";" $ map (quote . intercalate sep . map f. fst) rs
        
    writeFile (name++"dat.txt") sm
    writeFile (name++".m") . (++ unlines epilog) . unlines $
        [ "data=load(\""++name++"dat.txt\");"
        , "labels={"++s++"};"
        , "boxplot(data);"
        , "tics(\"x\", 1:size(data)(2),labels);"
--        , "axis([0,"++show (cols m+1)++",0.9,2]);"
        ]
    -- system ("octave -q "++name++".m") -- fix this!
    return ()

--------------------------------------------------------------------------------

exportPDF :: String -> FilePath -> FilePath -> Results -> [String]
          -> IO ExitCode
exportPDF sep name path exper epilog = do
    export sep
           name
           exper
           (epilog++["print -dsvg "++name++".svg"])
    script ["NAME":>name, "PATH":>path]
        [ "octave -q NAME.m"
        , "inkscape NAME.svg --export-pdf=NAME.pdf --export-area-drawing"
        , "rm NAME.m NAMEdat.txt NAME.svg"
        , "mv NAME.pdf PATH"
        ]

