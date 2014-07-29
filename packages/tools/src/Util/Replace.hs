{-# LANGUAGE ViewPatterns #-}

module Util.Replace(
    Rule(..), parseRules, ioReplace,
    include, ignore, define, quote, hsfile, hscolour, local
) where

import Data.List(isPrefixOf,intercalate)
--import System.Process(system)
--import System.Exit(ExitCode(..))
import Data.List.Split(splitOn)
import qualified Util.Text as T

--import Debug.Trace

--debug x = trace ("***> " ++ show x) x

{-
data Rule = String :> String
          | String :>> String
          | String :-> (String -> String)
          | String :->> (String -> String)
          | String :~> (String -> IO String)
          | String :~>> (String -> IO String)
          | Define
-}

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

--script :: [Rule] -> [String] -> IO ExitCode
--script rules ls = system . snd =<< ioReplace [] rules (unlines ls)


ignore :: Rule
ignore = "!IGNORE" :-> const (return ([],""))

quote :: Rule
quote = "!QUOTE" :-> h
  where
    h thing = return ([],"\""++replace ["\\"-:>"\\\\", "\""-:>"\\\""] thing ++"\"")

{-
include' :: Rule
include' = "!INCLUDE" :-> h
  where
    h (fileargs->(file,rules)) = do
        x <- readFile file
        let res | null rules = return ([],x)
                | otherwise  = ioReplace [] rules x
        res
    fileargs s = (file, parseRules rest)
      where
        w:ws = splitOn ":" s
        file = wu w
        rest = intercalate ":" ws        
-}

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



{-

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

-}

{-
splitOn ";" ""
print . snd =<< ioReplace (parseRules "w=W;g Y = bYb ; z=az; f X = aXa", "hello world! and z and f(35 and g[67] and f[4])")
print . snd =<< ioReplace [] (parseRules "z=az; az=y; y=zb") "tonto y"
print . snd =<< ioReplace (parseRules "g Y = bYb ; f X = aXa", "hello world! and z and f(35 and g[67] and f[4])")
print . snd =<< ioReplace [] (parseRules "w=W;g Y= bYb; f x = axa") "No empieza f(35 and g[67] and f[4])"
print . snd =<< ioReplace [] (parseRules "z=az; x=cy; y=bz") "tonto x"
print . snd =<< ioReplace [] (parseRules "f(0) = 1; f x = 1 + f[x]+f[0]") "f[3]"
print . snd =<< ioReplace [] ([ignore]) "hola !IGNORE(algo)word!"
print . snd =<< ioReplace [] ([ignore,include]) "hola !INCLUDE(borrar.txt: bonito=feo) word!"
print . snd =<< ioReplace [] ([ignore,include,define]) "Hello !INCLUDE(borrar.txt: bonito=feo) world!"
print . snd =<< ioReplace [] ([ignore,include,define]) "Hello !INCLUDE(borrar.txt: bonito=feo) world!"
-}

{-
matchString :: Rule -> String
matchString (x :>   _ ) = x
matchString (x :>>  _ ) = x
matchString (x :->  _ ) = x
matchString (x :->> _ ) = x
matchString (x :~> _  ) = x
matchString (x :~>> _ ) = x
matchString Define      = "!DEFINE"


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
ioReplace :: [Rule] -> String -> IO (String,[Rule])

ioReplace rs [] = return ([],rs)

ioReplace rs xs@(y:ys) = do
    m <- match rs xs
    case m of
        Nothing      ->
            do (res,ruls) <- ioReplace rs ys
               return (y:res, ruls)
{-        Just (lhs,n,rhs,newrules,rec) ->
            do  let rep = ioReplace (newrules++rs)
                a <- (if rec then (fmap fst) . rep else return) rhs
                b <- rep (drop n xs)
                return $ (a ++ b, newrules ++ rs)
-}

match :: [Rule] -> String -> IO (Maybe (String, Int, String, [Rule], Bool))
match [] _ = return Nothing

match ((lhs :> rhs):rs) x
    | lhs `isPrefixOf` x = (return . Just) (lhs,length lhs, rhs,[], False)
    | otherwise = match rs x

match ((lhs :>> rhs):rs) x
    | lhs `isPrefixOf` x = (return . Just) (lhs,length lhs, rhs,[], True)
    | otherwise = match rs x

match ((lhs :-> rhs):rs) x = match ((lhs :~> (return . rhs)):rs) x
match ((lhs :->> rhs):rs) x = match ((lhs :~>> (return . rhs)):rs) x

match ((lhs :~> rhs):rs) x
    | lhs `isPrefixOf` x = do
        res <- rhs arg
        (return . Just) (lhs, l, res, [],False)
    | otherwise = match rs x
  where
    (arg,l) = findArg lhs x

match ((lhs :~>> rhs):rs) x
    | lhs `isPrefixOf` x = do
        res <- rhs arg
        (return . Just) (lhs, l, debug res, [],True)
    | otherwise = match rs x
  where
    (arg,l) = findArg lhs x


match (Define:rs) x
    | "!DEFINE" `isPrefixOf` x =  (return . Just) ("!DEFINE", l, "", parseRules arg, False)
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
script rules ls = system . fst =<< ioReplace rules (unlines ls)

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

-}

