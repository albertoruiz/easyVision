import System.Process
import System.Directory
import Data.List.Split
import Data.List(isPrefixOf,isSuffixOf,intercalate,inits)
import Control.Applicative((<$>))
import Control.Monad(when)
import System.Environment
import Control.Arrow

main = do
    m:_ <- getArgs
    home <- getHomeDirectory
    let index = home ++ "/.cabal/share/doc/index.html"
    doc <- readFile index
    let entries = drop 2 $ map (f . take 2 . splitOn ">") $ splitOn "<span class=\"module\">" doc
        (s,m') = (head &&& last) (words m)
        m'' = init . tail $ m'
    --print (s,m'')
    case lookup m entries of
        Just p  -> putStrLn p
        Nothing -> case lookup m'' entries of
                      Nothing -> putStrLn $ "http://holumbus.fh-wedel.de/hayoo/hayoo.html?query="++m''
                      Just p  -> putStrLn $ p -- ++ "#v:"++s
        
f [a,b] = (takeWhile (/='<') b, tail . init $ dropWhile (/='\"') a)

