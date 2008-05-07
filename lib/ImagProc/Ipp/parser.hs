module Main where

import Text.ParserCombinators.Parsec
import System.Environment
import Data.List(tails,isPrefixOf)

type Argument = ([String], String)

data Header = Header { name :: String
                     , tyre :: [String]
                     , args :: [Argument]
                     , doc  :: String
                     } deriving Show

headers f = case parse parseHeaders "" f of
    Right l -> l
    Left s -> error (show s)

parseHeaders = many (try parseHeader)

parseHeader = do
    manyTill anyChar (try $ string "IPPAPI")
    symbol "("
    tr <- many ident
    symbol ","
    n <- ident
    symbol ","
    params <- between (symbol "(") (symbol ")") (sepBy (many ident) (char ','))
    symbol ")"
    let separate xs = (init xs, last xs)
    return $ Header { name = n, tyre = tr, args = map separate params , doc = "" }

ident = do 
    spaces
    id <- many1 (noneOf "() \n\t,;")
    spaces
    return id

symbol s = do
    spaces
    r <- string s
    spaces
    return r

main = do
    ipp <- getEnv "IPP"
    header:_ <- getArgs
    f <- readFile (ipp++"/include/"++header)
    let coms = comments f
    let hds = map (addComment coms) (headers f)
    print.length $ hds
    mapM_ shfun (take 5 . drop 1000 $ hds)
    putStr . unlines . map name $ hds


comments f = case parse parseComments "" f of
    Right l -> l
    Left s -> error (show s)

parseComments = many (try parseComment)

parseComment = do
    manyTill anyChar (try $ string "/*")
    r <- manyTill anyChar (try $ string "*/")
    return r

contained x = any (x `isPrefixOf`) . tails

addComment coms h = h {doc = clean x} where
    nm = adjust (name h)
    x = head . filter (contained nm) $ coms
    adjust = fst . break (=='_')

clean ('/':'/':'/':rest) = clean ('/':'/':rest)
clean ('/':'/':rest) = clean rest
clean (x:xs) = x: clean xs
clean x = x

shfun h = do
    putStrLn (replicate 70 '=')
    putStrLn (name h)
    putStrLn (replicate 35 '-')
    putStrLn (doc h)

--------------------------------------------

test = case parse testParser "" "aa bb,bb cc dd, cc , dd" of
    Right l -> l
    Left s -> error (show s)

testParser = do
    sepBy (many ident) (char ',')
