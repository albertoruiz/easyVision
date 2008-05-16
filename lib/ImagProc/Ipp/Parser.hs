module Parser(
    Argument, Header(..),
    getHeaders
) where

import Text.ParserCombinators.Parsec
import System.Environment
import Data.List(tails,isPrefixOf,inits,isSuffixOf)

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
    let hds = getHeaders f
    print.length $ hds
    mapM_ shfun hds
    --putStr . unlines . map name $ hds

getHeaders :: String -> [Header]
getHeaders f = map (addComment coms) (headers f)
    where coms = comments f


comments f = case parse parseComments "" f of
    Right l -> l
    Left s -> error (show s)

parseComments = many (try parseComment)

parseComment = do
    manyTill anyChar (try $ string "/*")
    r <- manyTill anyChar (try $ string "*/")
    return r

contained x = any (x `isPrefixOf`) . tails

addComment coms h = h {doc = ok x} where
    nm = adjust (name h)
    x = head . (++["Not Found"]) . filter (contained nm) $ coms
    adjust = fst . break (=='_')
    ok = getPurpose . clean

clean ('/':'/':'/':rest) = clean ('/':'/':rest)
clean ('/':'/':rest) = clean rest
clean (x:xs) = x: clean xs
clean x = x

getPurpose = beforeLast "\n" . entre "Purpose:" ":"

entre s1 s2 = before s2 . after s1

after  s =     drop (length s) . head . (++[""]) . snd . break (s `isPrefixOf`) . tails
before s = dropBack (length s) . head . (++[""]) . snd . break (s `isSuffixOf`) . inits

dropBack k s = take (length s - k) s

beforeLast what s = reverse $ after (reverse what) (reverse s)

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
