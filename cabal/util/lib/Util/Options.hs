-- | Command line options

module Util.Options (
    getRawOption,
    getOption,
    optionString,
    getFlag,
    hasValue,
    maybeOption,
    cleanOpts,
    nextFilename
) where

import Data.List(isPrefixOf)
import System.Environment(getArgs)
import System.Directory(getDirectoryContents)

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
    return (any (isPrefixOf name) args)

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

