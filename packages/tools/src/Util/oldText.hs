{-# LANGUAGE ViewPatterns #-}

module Util.Replace(
    Rule(..),
    ioReplace,
    include, ignore, parseRules, includerec,
    hsfile, hsfilepart, hsfiledef, hsfragment,
    script
) where

import Data.List(isPrefixOf,isInfixOf,intercalate)
import System.Process(system)
import System.Exit(ExitCode(..))
import Data.List.Split(splitOn)
import qualified Util.Text as T

import Debug.Trace

debug x = trace ("***> " ++ show x) x


data Rule = String :> String
          | String :>> String
          | String :-> (String -> String)
          | String :->> (String -> String)
          | String :~> (String -> IO String)
          | String :~>> (String -> IO String)
          | Define

(-:>) :: String -> String -> T.Rule
(-:>) = (T.:>)

{-
isRec (_ :>> _)  = True
isRec (_ :->> _) = True
isRec (_ :~>> _) = True
isRec _          = False


instance Show Rule
  where
    show (a :> b)  = show a ++" :> "++show b
    show (a :>> b) = show a ++" :>> "++show b
    show (a :-> _) = show a ++" :-> pure fun"
    show (a :->> _) = show a ++" :->> pure fun"
    show (a :~> _) = show a ++" :~> IO fun"
    show (a :~>> _) = show a ++" :~>> IO fun"
    show Define    = "Define"
-}

--------------------------------------------------------------------------------

-- IO transformations
ioReplace :: [Rule] -> String -> IO String

ioReplace _ [] = return []

ioReplace rs xs@(y:ys) = do
    m <- match rs xs
    case m of
        Nothing      -> (y:) `fmap` ioReplace rs ys
        Just (n,rhs,newrules,rec) ->
            do  let rep = ioReplace (newrules++rs)
                a <- (if rec then (fmap id) . rep else return) rhs
                b <- rep (drop n xs)
                return $ a ++ b

match :: [Rule] -> String -> IO (Maybe (Int, String, [Rule], Bool))
match [] _ = return Nothing

match ((lhs :> rhs):rs) x
    | lhs `isPrefixOf` x = (return . Just) (length lhs, rhs,[], False)
    | otherwise = match rs x

match ((lhs :>> rhs):rs) x
    | lhs `isPrefixOf` x = (return . Just) (length lhs, rhs,[], True)
    | otherwise = match rs x

match ((lhs :-> rhs):rs) x = match ((lhs :~> (return . rhs)):rs) x
match ((lhs :->> rhs):rs) x = match ((lhs :~>> (return . rhs)):rs) x

match ((lhs :~> rhs):rs) x
    | lhs `isPrefixOf` x = do
        res <- rhs arg
        (return . Just) (l, res, [],False)
    | otherwise = match rs x
  where
    (arg,l) = findArg lhs x

match ((lhs :~>> rhs):rs) x
    | lhs `isPrefixOf` x = do
        res <- rhs arg
        (return . Just) (l, debug res, [],True)
    | otherwise = match rs x
  where
    (arg,l) = findArg lhs x


match (Define:rs) x
    | "!DEFINE" `isPrefixOf` x =  (return . Just) (l, "", parseRules arg, False)
    | otherwise = match rs x
  where
    (arg,l) = findArg "!DEFINE" x

--------------------------------------------------------------------------------

findArg :: String -> String -> (String, Int)
findArg lhs x | null rest = error $ "replace, no closing bracket for " ++ lhs
              | otherwise = {-debug "REST" (const rest) -} (arg,l)
  where
    aux = drop (length lhs) $ x
    sep1 | null aux  = error $ "replace, no opening bracket for " ++ lhs
         | otherwise = head aux
    (arg,rest) = span (/=sep2) . drop 1 $ aux
    l = length lhs + length arg + 2
    sep2 = case sep1 of
        ' ' -> '\n'
        ':' -> '\n'
        '(' -> ')'
        '[' -> ']'
        '<' -> '>'
        s   ->  s

wu :: String -> String
wu = unwords . words

parseRules :: String -> [Rule]
parseRules x = r1 x ++ r2 x
  where
    r1 = concatMap (t False "=") . ok "->" . filter (/=[""]) . map (splitOn "=") . splitOn ";"
    r2 = concatMap (t True "->") . ok "="  . filter (/=[""]) . map (splitOn "->") . splitOn ";"
    t rec sep (l:r:rest) = [mkD rec l (esp (wu r'))]
      where
        r' = intercalate sep (r:rest)
    t _ _ _ = []
    ok sep = filter (not .isInfixOf sep . head)

mkD :: Bool -> String -> String -> Rule      
mkD rec hd body
    | null args = esp name `op1` body
    | otherwise = name `op2` \x -> T.replace (rules x) body
  where
   name:args = words hd
   rules x = zipWith (-:>) args (splitOn "," (esp x))
   op1 = if rec then (:>>) else (:>)
   op2 = if rec then (:->>) else (:->)

esp :: String -> String
esp = T.replace [ "!SEMICOLON" -:> ";"
              , "!COMMA" -:> ";"
              , "!ARROW" -:> "->"
              , "!EQUAL" -:> "="
              , "\\\\" -:> "\\"
              , "\\n" -:> "\n"
              , "!SPACE" -:> " "
              ]

-- args "kk.txt:X   ->  20 , Y   ->  2 e  "
-- args "kk.txt:X   ->  20"
-- args "kk.txt"
-- args "kk.txt:X   ->  20, hello->goodbye->no"
-- replace [mkD "!ADJ X Y" "tonto, X, Y, pero"] "yo soy !ADJ(listo,caca) hello!"
-- replace (parseRules "!ADJ X Y = tonto!COMMA X!COMMA Y!COMMA pero") "yo soy !ADJ(listo,caca) hello!"
-- replace (parseRules "KKK= <pre id=\"hsex\"> bla bla </pre>") "yo soy KKK hola"
-- replace (parseRules "PRE X Y = <pre X> Y</pre>, EXAMPLE X -> PRE(hsr!COMMAX)") "EXAMPLE(hola)"
-- ioReplace (include:parseRules "COSA X -> !INCLUDE(X)") "una COSA(kk.hs)"

--------------------------------------------------------------------------------

script :: [Rule] -> [String] -> IO ExitCode
script rules ls = system =<< ioReplace rules (unlines ls)

--------------------------------------------------------------------------------

ignore :: Rule
ignore = "!IGNORE" :-> const ""


include :: Rule
include = "!INCLUDE" :~> h
  where
    h (fileargs->(file,rules)) = do
        x <- readFile file
        --print x
        let res | null rules =  return x
                | otherwise  = ioReplace rules x
        -- putStr $ "==> include:" ++ res
        res

    fileargs s = (file, parseRules rest)
      where
        w:ws = splitOn ":" s
        file = wu w
        rest = intercalate ":" ws        

includerec :: Rule
includerec = "!IMPORT" :~>> readFile

--------------------------------------------------------------------------------

hsfile :: Rule
hsfile = "!HSFILE" :~> \f -> (return . T.hsCol . addName f) =<< readFile f
  where
    -- addName f xs = replicate 0 ' ' ++ "-- "++ drop 3 f++"\n\n"++ xs
    addName _ = id

hsfilepart :: Rule
hsfilepart = "!HSPART" :~> g
  where
    g fs = do
        let filename:rest = words fs
            section = unwords rest
        f <- readFile filename
        let [_,x0] = splitOn section f
        let x:_ = splitOn "\n\n\n" (dropWhile (=='\n') x0)
        return (T.hsCol x)

hsfiledef :: Rule
hsfiledef = "!HSDEF" :~> g
  where
    g fs = do
        let filename:rest = words fs
            section = unwords rest
        f <- readFile filename
        let _:x0 = splitOn section f
        let x:_ = splitOn "\n\n\n" (section ++ intercalate section x0)
        return (T.hsCol x)


-- code explicitly given
hsfragment :: Bool -> Rule
hsfragment mode = "!HSCODE" :-> ((if mode then id else T.hsCol) . T.replace rules)
  where
    rules = ["{\\-"-:>"{-", "-\\}"-:>"-}"]

