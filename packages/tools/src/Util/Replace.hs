{-# LANGUAGE ViewPatterns #-}

module Util.Replace(
    Rule(..), parseRules, ioReplace,
    include, ignore, define, quote, hsfile, hscolour, local
) where

import Data.List(isPrefixOf,intercalate)
import Data.List.Split(splitOn)
import qualified Util.Text as T

data Rule = String :>  IO ([Rule], String)
          | String :-> (String -> IO ([Rule], String))

(-:>) :: String -> String -> T.Rule
(-:>) = (T.:>)
replace :: [T.Rule] -> String -> String
replace = T.replace

wu :: String -> String
wu = unwords . words

parseRules :: String -> [Rule]
parseRules = map (t . splitOn "=") . filter (not.null) . splitOn ";"
  where
    t (l:r:rest) =  mkRule l r'
      where
        r' = esp . wu . intercalate "=" $ r:rest
    t xs = error $ "parseRules: " ++ intercalate "=" xs

mkRule :: String -> String -> Rule
mkRule hd body
    | null args = esp name :> return ([],body)
    | otherwise = name :-> \x -> return ([], replace (rules x) body)
  where
    name:args' = words hd
    (sep,args)
        | map (take 1) (take 1 args') == ["'"] = (tail (head args'), tail args')
        | otherwise = (" ",args')
    rules x = zipWith (-:>) args (splitNOn (length args) sep (esp x))

splitNOn :: Eq a => Int -> [a] -> [a] -> [[a]]
splitNOn n sep = f . splitOn sep
  where
    f xs = take (n-1) xs ++ [intercalate sep (drop (n-1) xs)]

esp :: String -> String
esp = T.replace [ "!SEMICOLON" -:> ";"
              , "!COMMA" -:> ";"
              , "!ARROW" -:> "->"
              , "!EQUAL" -:> "="
              , "\\\\" -:> "\\"
              , "\\n" -:> "\n"
              , "!SPACE" -:> " "
              ]

ioReplace :: [String] -> [Rule] -> String -> IO ([Rule], String)

ioReplace _ rs [] = return (rs, [])

ioReplace prev rs (xs@(y:ys)) = do
    m <- match rs prev rs xs
    --putStrLn $ "*" ++ show prev
    case m of
        Nothing ->
            do (nruls, res) <- ioReplace prev rs ys
               return (nruls, y:res)
        Just ("", n, rhs, newrules) ->
            do (brules,b) <- ioReplace prev (newrules++rs) (drop n xs)
               return $ (brules, rhs++b)
        Just (lhs, n, rhs, newrules) ->
            do (arules,a) <- ioReplace (lhs:prev) (newrules++rs) rhs
               (brules,b) <- ioReplace prev arules (drop n xs)
               return $ (brules, a++b)



match :: [Rule] -> [String] -> [Rule] -> String -> IO (Maybe (String, Int, String,[Rule]))
match _ _ [] _ = return Nothing

match ars prev ((lhs :> rhs):rs) x
    | ('#':lhs) `isPrefixOf`x && not (lhs `elem` prev) = do
        (nruls,res) <- rhs
        (return . Just) ("", length lhs +1, res, nruls)

    | lhs `isPrefixOf`x && not (lhs `elem` prev) = do
        (nruls,res) <- rhs
        (return . Just) (lhs, length lhs, res, nruls)

    | otherwise = match ars prev rs x


match ars prev ((lhs :-> rhs):rs) x
    | hold && not ((lhs ++" "++ arg) `elem` prev) = do
        (_nruls1,evarg) <- ev arg
        (nruls,res) <- rhs evarg
        (return . Just) ("", l+1, res, nruls)  

    | lhs `isPrefixOf` x && not ((lhs ++" "++ arg) `elem` prev) = do
        (_nruls1,evarg) <- ev arg
        (nruls,res) <- rhs evarg
        (return . Just) (fapp, l, res, nruls)
          
    | otherwise = match ars prev rs x

  where
    hold = ('#':lhs) `isPrefixOf` x
    x' = if hold then tail x else x
    (fapp, arg,l,recur) = findArg lhs x'
    ev a | not recur = return (ars, a)
         | otherwise = ioReplace prev ars a


findArg :: String -> String -> (String, String, Int, Bool)
findArg lhs x | null rest && sep2 /= '\n' = error $ "replace, no closing bracket for " ++ lhs
              | otherwise = {-debug "REST" (const rest) -} (fapp, arg,l,recur)
  where
    aux1 = drop (length lhs) $ x
    recur = take 1 aux1 == ">"
    aux2 = if recur then tail aux1 else aux1
    sep1 | null aux2  = error $ "replace, no opening bracket for " ++ lhs
         | otherwise = head aux2
    (arg,rest) = span (/=sep2) . drop 1 $ aux2
    l = length lhs + length arg + 2 + if recur then 1 else 0
    sep2 = case sep1 of
        ' ' -> '\n'
        ':' -> '\n'
        '(' -> ')'
        '[' -> ']'
        '<' -> '>'
        '{' -> '}'
        s   ->  s
    fapp = lhs ++" "++arg

--------------------------------------------------------------------------------

ignore :: Rule
ignore = "!IGNORE" :-> const (return ([],""))


quote :: Rule
quote = "!QUOTE" :-> h
  where
    h thing = return ([],"\""++replace ["\\"-:>"\\\\", "\""-:>"\\\""] thing ++"\"")


define :: Rule
define = "!DEFINE" :-> h
  where
    h (parseRules -> rules) = return (rules,"") 


hsfile :: Rule
hsfile = "!HSFILE" :-> g
  where
    g fs = do
        let filename:rest = words fs
            section = unwords rest
        f <- readFile filename
        let _:x0 = splitOn section f
            x:_ = splitOn "\n\n\n" (section ++ intercalate section x0)
            y | null rest = f
              | otherwise = x
        return ([], T.hsCol y)


include :: Rule
include = "!INCLUDE" :-> g
  where
    g fs = do
        let filename:rest = words fs
            section = unwords rest
        f <- readFile filename
        let _:x0 = splitOn section f
            x:_ = splitOn "\n\n\n" (section ++ intercalate section x0)
            y | null rest = f
              | otherwise = x
        return ([], y)


hscolour :: Bool -> Rule
hscolour mode = "!HSCOLOUR" :-> g
  where
    g thing = return ([], f thing)
    f = if mode then id else T.hsCol

local :: Rule
local = "!REPLACE" :-> h
  where
    h (args->(thing,rules)) = do
        (_,res) <- ioReplace [] rules thing
        return ([],res)
    args s = (thing, parseRules rest)
      where
        thing:ws = splitOn "!WITH" s
        rest = intercalate "!WITH" ws        

