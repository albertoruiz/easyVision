import System.Process
import System.Directory
import Data.List.Split
import Data.List(isPrefixOf,isSuffixOf,intercalate)
import Control.Applicative((<$>))
import Control.Monad(when)
import System.Environment

main = do
    home <- getEnv "HOME"
    let fconfig = home++"/.cabal/config"
    ok <- doesFileExist fconfig
    when (not ok) $ error "missing ~/.cabal/config"
    config <- readFile fconfig
    let lrepo = filter ((["local-repo:"]==). take 1) (map words $ lines config)
    when (null lrepo) $ error "missing local-repo in ~/.cabal/config"
    let repo = last . head $ lrepo
    ldist <- filter (".tar.gz" `isSuffixOf`) <$> getDirectoryContents "dist"
    lcabal <- filter (".cabal" `isSuffixOf`) <$> getDirectoryContents "."
    when (null ldist)  $ error "missing distribution tar.gz"
    when (null lcabal) $ error "mising cabal file"
    let dist    = head ldist
        cabal   = head lcabal
        namever = splitOn "-" (head $ splitOn ".tar.gz" dist)
        ver     = last namever
        name    = intercalate "-" (init namever)
        route   = intercalate "/" [repo,name,ver]
    system $ "mkdir -p " ++ route
    system $ "cp dist/" ++ dist ++" "++ route
    system $ "cp " ++ cabal ++" "++ route
    system $ "rm -f "++repo++"/00-index.cache "++repo++"/00-index.tar"
    system $ "cd "++repo++"; tar cf 00-index.tar *"
    putStrLn $ name ++ "-" ++ ver ++ " added to local repo"


    

