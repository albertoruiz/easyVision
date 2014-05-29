module Util.Text(
    Rule(..),
    replace, ioReplace,
    script,
    wordAfter, lineAfter, include, codefile,
    myhscols, hsCol, hsfile, hsfilepart, hsfiledef, hsfragment
) where

import Data.List(isPrefixOf,isInfixOf)
import System.Process(system)
import System.Exit(ExitCode(..))
import Control.Arrow
import Data.Char(toLower)
import Data.Function(on)
import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise
import Data.List.Split(splitOn)

data Rule = String :> String
          | String :-> (String -> String)
          | String :~> (String -> IO String)

--------------------------------------------------------------------------------

-- pure transformations
replace :: [Rule] -> String -> String

replace _ [] = []

replace rs xs@(y:ys) =
    case pmatch rs xs of
        Nothing      -> y : replace rs ys
        Just (n,rhs) -> rhs ++ replace rs (drop n xs)

pmatch :: [Rule] -> String -> Maybe (Int, String)

pmatch [] _ = Nothing

pmatch ((lhs :> rhs):rs) x
    | lhs `isPrefixOf` x = Just (length lhs, rhs)
    | otherwise = pmatch rs x


pmatch ((_lhs :~> _rhs):rs) x = pmatch rs x

pmatch ((lhs :-> rhs):rs) x
    | lhs `isPrefixOf` x = Just (l, rhs arg)
    | otherwise = pmatch rs x
  where
    (arg,l) = findArg lhs x

--------------------------------------------------------------------------------

findArg :: String -> String -> (String, Int)
findArg lhs x | null rest = error $ "replace, no closing bracket for " ++ lhs
              | otherwise = {-debug "REST" (const rest) -} (arg,l)
  where
    aux = dropWhile (==' ') . drop (length lhs) $ x
    sep1 | null aux  = error $ "replace, no opening bracket for " ++ lhs
         | otherwise = head aux
    (cmd,(arg,rest)) = (id *** span (/=sep2). drop 1) . span (/=sep1) $ x
    l = length cmd + length arg + 2
    sep2 = case sep1 of
                            '(' -> ')'
                            '[' -> ']'
                            '<' -> '>'
                            s   ->  s
--------------------------------------------------------------------------------

-- IO transformations
ioReplace :: [Rule] -> String -> IO String

ioReplace _ [] = return []

ioReplace rs xs@(y:ys) = do
    m <- match rs xs
    case m of
        Nothing      -> (y:) `fmap` ioReplace rs ys
        Just (n,rhs) -> (rhs ++) `fmap` ioReplace rs (drop n xs)

match :: [Rule] -> String -> IO (Maybe (Int, String))
match [] _ = return Nothing

match ((lhs :> rhs):rs) x
    | lhs `isPrefixOf` x = return (Just (length lhs, rhs))
    | otherwise = match rs x

match ((lhs :-> rhs):rs) x = match ((lhs :~> (return . rhs)):rs) x

match ((lhs :~> rhs):rs) x
    | lhs `isPrefixOf` x = (Just . (,) l) `fmap` rhs arg
    | otherwise = match rs x
  where
    (arg,l) = findArg lhs x

script :: [Rule] -> [String] -> IO ExitCode
script rules ls = system =<< ioReplace rules (unlines ls)

--------------------------------------------------------------------------------
-- misc tools

wordAfter :: String -> String -> String
wordAfter w = unwords . take 1 . drop 1 . dropWhile (d w) . words
  where
    d = (/=) `on` map toLower

lineAfter :: String -> String -> String
lineAfter w = unwords
            . map (unwords . drop 1 . dropWhile (d w) . words)
            . take 1 . dropWhile f . lines
  where
    d = (/=) `on` map toLower
    f = not . isInfixOf w . map toLower

include :: Rule
include = "INCLUDE" :~> readFile

codefile :: Rule
codefile = "CODEFILE" :~> \f -> (return . (\s -> "<pre><code>" ++ s ++ "</code></pre>")) =<< readFile f

--------------------------------------------------------------------------------

-- hscolour tools

myhscols :: ColourPrefs
myhscols = defaultColourPrefs
    { comment = [Italic, Dim, Foreground (Rgb 128 128 128)]
    , keyword = [Bold]
    , varop = [Foreground Green]
    , keyglyph = [Foreground Black]
    , layout = [Normal]
    , string = [Foreground (Rgb 160 0 0)]
    , number = [Foreground (Rgb 128 80 0)]
    , conid = [Foreground Blue, Bold]
    , definition = [Foreground Blue]
    , selection = [Foreground Green]
    , cpp = [Foreground Green]
    }

hsfile :: Rule
hsfile = "HSFILE" :~> \f -> (return . hsCol . addName f) =<< readFile f
  where
    addName f xs = replicate 0 ' ' ++ "-- "++ drop 3 f++"\n\n"++ xs

hsfilepart :: Rule
hsfilepart = "HSPART" :~> g
  where
    g fs = do
        let filename:rest = words fs
            section = unwords rest
        f <- readFile filename
        let [_,x0] = splitOn section f
        let x:_ = splitOn "\n\n\n" (dropWhile (=='\n') x0)
        return (hsCol x)

hsfiledef :: Rule
hsfiledef = "HSDEF" :~> g
  where
    g fs = do
        let filename:rest = words fs
            section = unwords rest
        f <- readFile filename
        let _:x0:_ = splitOn section f
        let x:_ = splitOn "\n\n\n" (section ++ x0)
        return (hsCol x)



hsfragment :: Rule
hsfragment = "HSCODE" :-> hsCol

hsCol :: String -> String
hsCol = hscolour HTML myhscols False True "" False

