{-# LANGUAGE ViewPatterns #-}

module Util.Text(
    Rule(..), replace,
    dimString, redString, wordAfter, lineAfter,
    hsCol
) where

import Data.List(isPrefixOf,isInfixOf)
import Data.Char(toLower)
import Data.Function(on)
import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise

data Rule = String :> String

replace :: [Rule] -> String -> String

replace _ [] = []

replace rs xs@(y:ys) =
    case match rs xs of
        Nothing      -> y : replace rs ys
        Just (n, rhs) -> rhs ++ replace rs (drop n xs)

match :: [Rule] -> String -> Maybe (Int, String)

match [] _ = Nothing

match ((lhs :> rhs):rs) x
    | lhs `isPrefixOf` x = Just (length lhs, rhs)
    | otherwise = match rs x


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

hsCol :: String -> String
hsCol = hscolour HTML myhscols False True "" False

--------------------------------------------------------------------------------
-- misc tools

dimString :: String -> String
dimString s = "\^[[2m"++s++"\^[[0m"

redString :: String -> String
redString s = "\^[[0;31m"++s++"\^[[0m"


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

