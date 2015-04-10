-- | Command line options

module Util.Options (
    getRawOption,
    getOption,
    getSubOption,
    hasSubOption,
    optionString,
    optionFromFile,
    getFlag,
    hasValue,
    maybeOption,
    cleanOpts,
    nextFilename
) where

import Data.List(isPrefixOf)
import Data.List.Split(splitOn)
import System.Environment(getArgs)
import System.Directory(getDirectoryContents)
import Control.Monad(join,when)

-- | extracts an option from the command line. From --alpha=1.5 we get 1.5::Double.
getOption :: Read a
          => String  -- ^ option name (e.g. \"--alpha\")
          -> a       -- ^ default value
          -> IO a    -- ^ result
getOption name def = do
    mbopt <- getRawOption name
    let v = case mbopt of
                Nothing -> def
                Just s  -> read s
    return v

-- | searches for an optional argument
getRawOption :: String -- ^ option name
             -> IO (Maybe String)
getRawOption name = do
    args <- getArgs
    when ("--options" `elem` args) (putStrLn name)
    let opts = filter ((name++"=") `isPrefixOf`) args
    return $ case opts of
        [] -> Nothing
        xs -> Just (val (last xs))
  where val = tail . dropWhile (/= '=')


maybeOption :: (Read a) => String -> IO (Maybe a)
maybeOption name = fmap (fmap read) (getRawOption name)

-- | checks if --option=something has been given in the command line.
hasValue :: String -- ^ option name, including the dashes (e.g. \"--opt\").
         -> IO Bool
hasValue name = do
    x <- getRawOption name
    return $ case x of
        Nothing -> False
        Just _  -> True

-- | checks if --option or --option=something has been given in the command line.
getFlag :: String -- ^ option name, including the dashes (e.g. \"--opt\").
        -> IO Bool
getFlag name = do
    args <- getArgs
    when ("--options" `elem` args) (putStrLn name)
    return $ name `elem` args || any ((name ++ "=") `isPrefixOf`) args

-- | Special version of 'getOption' for strings, without the need of the quotes required by @read@.
optionString :: String -- ^ option name
             -> String -- ^ default value
             -> IO String
optionString name def = fmap (maybe def id) (getRawOption name)

cleanOpts :: [String] -> [String]
cleanOpts = filter (not . isPrefixOf "-")

-- | (without extension)
nextFilename :: String -> IO String
nextFilename prefix = do
    fs <- getDirectoryContents "."
    let n = 1+ length (filter (prefix `isPrefixOf`) fs)
        sn = show n
        k = 3 - length sn
        shj = replicate k '0' ++ sn
    return $ prefix ++ "-" ++ shj

-- | similar to getOption, but the value is read from a file
optionFromFile
    :: Read a
    => String  -- ^ option name for the filename
    -> a       -- ^ default value
    -> IO a    -- ^ result
optionFromFile option x = maybe x id <$> join (fmap sequenceA $ (fmap (fmap read . readFile)) <$> getRawOption option)

parseSubOptions :: String -> [(String,String)]
parseSubOptions = map ((\[k,v] -> (k,v)) . take 2 . splitOn "=" . (++"=")) . filter (not .null) . splitOn ":"


getSubOption :: String -> String -> String -> String
getSubOption a name def = maybe def id $ lookup name (parseSubOptions a)

hasSubOption :: String -> String -> Bool
hasSubOption a name = lookup name (parseSubOptions a) /= Nothing

