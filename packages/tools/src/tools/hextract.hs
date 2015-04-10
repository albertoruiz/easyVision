{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim(ParsecT)
import Data.Functor.Identity(Identity)

import System.Environment(getArgs)
import Data.List(partition)


data Fragment
    = Normal  String
    | Md      String
    | Mdgroup String
    deriving (Show)

frags :: [(String, String, String -> Fragment)]
frags =
    [ ("{- !MARKDOWN","-}",Mdgroup)
    , ("{- !LaTeX","-}",Mdgroup)
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

--test = fragments "ab c d e -- eval: tonto \n sd {- MARKDOWN \nkk  tonto hask: hola\n -} adiós"

wu :: String -> String
wu = unwords . words

render :: Fragment -> String

render (Mdgroup t) = ("\n"++) .  (++"\n") . unlines . tail . init .  lines $ t

render (Md t) = wu t ++ "\n"

render (Normal t) = t


main :: IO ()
main = do
    [inhs,ouths,outmd] <- getArgs
    f <- readFile inhs
    let frgs = fragments f
        isMd (Normal _)   = False
        isMd _ = True
        (md,hs) = partition isMd frgs
        
    --print frgs
     
    writeFile outmd . unlines . map render $ md
    writeFile ouths . unlines . map render $ hs

