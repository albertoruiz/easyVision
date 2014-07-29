{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim(ParsecT)
import Data.Functor.Identity(Identity)
import Control.Applicative((<$>))
import Data.List.Split(splitOn)
import Data.List(partition,intercalate)
import Util.Text


data Fragment = Normal  String
              | Expr    String
              | Command String
              | PreCmd  String
              | Def     String
              | Prog    String
              | NoProg  String
              | Wrong   String
              | Raw     String
              | Eval    String
              | ShEval  String
              | Print   String
              deriving (Show)

frags :: [(String, String, String -> Fragment)]
frags =
    [ --("\\eval{","}",Eval)
--    , ("\\evalsh{","}",ShEval)
--    , ("\\print{","}",Print),
      ("\\perform{","}",Print)
--    , ("!eval","\n",Expr)
--    , ("!print","\n",Command)
--    , ("!pprint","\n",PreCmd)
--    , ("!def","\n",Def)
--    , ("!raw","\n",Raw)
--    , ("!wrong","\n",Wrong)
    , ("!CODE","!END",Prog)
    , ("!HCODE","!END",NoProg)
    ]

spe :: (String, String, String -> Fragment) -> Parsr Fragment
spe (open,close,mk) = mk <$> do
    string open
    manyTill anyChar (try (string close))

special :: Parsr Fragment
special = choice . map (try . spe) $ frags

gparse :: Parsr [Fragment] -> String -> [Fragment]
gparse p f = case parse p "" f of
    Right l -> l
    Left s -> error (show s)

fragments :: String -> [Fragment]
fragments = gparse p
  where
    p = do
        x <- normal
        xs <- many pair
        return (x:concat xs)
    pair = do
           s <- special
           n <- normal
           return [s,n]

type Parsr t = ParsecT String () Identity t

normal :: Parsr Fragment
normal = Normal <$> do
    a  <- anyChar
    bs <- manyTill anyChar (choice something)
    return (a:bs)
  where
   something = (eof >> return "") : start
   start = map (try . lookAhead . string) openings
   openings = [ op | (op,_,_) <- frags ]

--test = fragments "ab c d e <hask> tonto </hask> sd <hask p> kk </hask> tonto hask: hola\n adiós"
-- test


wu :: String -> String
wu = unwords . words

render :: Fragment -> String
render (Normal t) = "    putStrLn "++show t


render (Eval t) = directEval t

render (Print t) = directPrint t

render (ShEval t) = "    putStr ("++show t ++ "++\" = \"); print ("++t++")"

render (Expr t) = unlines
    [ lit  (wu t)
    , eval (wu t)
    ]
  where


render (Command t) = unlines
    [ lit  (wu t)
    , peval (wu t)
    ]

render (PreCmd t) = unlines
    [ lit  t'
    , peval $ wu f ++" ("++t'++")"
    ]
  where
    f:ts = splitOn "$" t
    t' = (wu . intercalate " $ ") ts

render (Def t) = unlines
    [ litd (wu t)
    , "    " ++ wu t
    ]

render (Wrong t) = lit (wu t)

render (Raw t) = unlines
    [ 
      raweval (wu t)
    ]

render (NoProg _) = ""
render (Prog p) = "\n    putStrLn \"" ++ (init. tail . show . hsCol) p++ "\""

rawrender :: Fragment -> String
rawrender (Prog p)   = p ++ "\n"
rawrender (NoProg p) = p ++ "\n"
rawrender (_) = error "rawrender on unexpected fragment"

--code a = unlines [lit a , eval a]

escape :: String -> String
escape = replace ["\\":>"\\\\","\"":>"\\\""]

pre :: String -> String
pre "" = "<pre>"
pre x  = "<pre id=\\\""++x++"\\\">"

lit :: String -> String
lit a = "\n    putStrLn \""++pre "hsc" ++ escape a++ "</pre>\""

litd :: String -> String
litd a = "\n    putStrLn \""++pre "hsd" ++escape a++ "</pre>\""

peval :: String -> String
peval a = unlines [ "    putStr \""++ pre "hsr" ++"\""
                  , "    " ++ a
                  , "    putStr \"</pre>\"" 
                  ]

raweval :: String -> String
raweval a = unlines [ "    putStr \""++ pre "hsno" ++"\""
                  , "    " ++ a
                  , "    putStr \"</pre>\"" 
                  ]

eval :: String -> String
eval a = "    putStr \""++ pre "hsr"++"\"; print ("++a++"); putStrLn \"</pre>\"\n" 

directEval :: String -> String
directEval  a = "    print ("++a++")"

directPrint :: String -> String
directPrint a = "    "++a

main :: IO()
main = do
    f <- getContents
    let frgs = fragments f
        isProg (Prog _)   = True
        isProg (NoProg _) = True
        isProg _ = False
        (fpre,_post) = partition isProg frgs
    --putStrLn "module Main where"
    putStrLn . replace ["\nmain =" :> "\nmainSplice ="] . unlines . map rawrender $ fpre
    putStrLn "\n----------\n\nmain = do\n"
    putStrLn . unlines . map render $ frgs
    --print frgs

