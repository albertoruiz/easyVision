module Util.Time(cTime,ioTime,formattedTime,formattedDate) where

import System.IO(hPutStr, hPutStrLn, stderr)
import System.Time
import System.Locale
import System.IO.Unsafe(unsafePerformIO)
import Control.DeepSeq

dimString :: String -> String
dimString s = "\^[[2m"++s++"\^[[0m"

cTime :: NFData a => String -> (t -> a) -> t -> a
cTime msg f x = unsafePerformIO $ do
    t0 <- getClockTime
    let y = f x
    hPutStr stderr ((\()->"") (rnf y))
    t1 <- getClockTime
    hPutStrLn stderr (dimString (msg ++ ": "++ show (dt t1 t0)++" ms"))
    return y
  where
    dt (TOD s1 p1) (TOD s0 p0) = ((s1-s0) * 10^(12::Int) + (p1-p0) ) `div` 10^(9::Int)

ioTime :: String -> IO () -> IO ()
ioTime msg act = do
    t0 <- getClockTime
    act
    t1 <- getClockTime
    putStrLn $ msg ++ ": "++ show (dt t1 t0)++" ms"
  where
    dt (TOD s1 p1) (TOD s0 p0) = ((s1-s0) * 10^(12::Int) + (p1-p0) ) `div` 10^(9::Int)

--------------------------------------------------------------------------------

formattedTime :: IO String
formattedTime = do
    t <- getClockTime >>= toCalendarTime
    return $ formatCalendarTime
               defaultTimeLocale
               (iso8601DateFormat (Just "%H-%M-%S"))
               t

formattedDate :: IO String
formattedDate = takeWhile (/='T') <$> formattedTime

--------------------------------------------------------------------------------

