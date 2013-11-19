import System.Process
import System.Directory
import Data.List.Split
import Data.List(isPrefixOf,isSuffixOf,intercalate,inits)
import Control.Applicative((<$>))
import Control.Monad(when)
import System.Environment
import Control.Arrow

isProperSuffixOf s x = x /= s && isSuffixOf s x

filesAndDir p = do
    fs <- getDirectoryContents p
    return (fs,p)

main = do
    current <- getCurrentDirectory
--    print current
    let paths = filter (not . null)
              . reverse
              . map (intercalate "/")
              . inits
              . splitOn "/"
              $ current
--    print paths
    look <- filter (not.null.fst)
             <$> mapM (((take 1 .filter (isProperSuffixOf ".cabal") *** id) <$>) . filesAndDir)
                      paths
--    print look
    when (null look) (error "cabal file not found")
    putStrLn (snd $ head look)

