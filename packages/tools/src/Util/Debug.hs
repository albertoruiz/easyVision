module Util.Debug(
    errMsg, debug, debugMat,
    impossible, assert, warning,
    trap, (///), (//>)
) where


import Numeric.LinearAlgebra.HMatrix
import Debug.Trace
import System.IO(hPutStrLn, stderr)
import Data.Binary
import System.IO.Unsafe(unsafePerformIO)
import Util.Text


debug :: (Show a) => String -> (a1 -> a) -> a1 -> a1
debug msg f x = trace (dimString (msg ++ ": " ++ show (f x))) x

errMsg :: String -> IO ()
errMsg = hPutStrLn stderr . dimString

debugMat :: String -> Int -> (t -> Matrix Double) -> t -> t
debugMat msg dec f x = trace (dimString (msg ++ " " ++ init (dispf dec (f x)))) x

-- | used to avoid incomplete patterns
impossible :: String -> a
impossible msg = error ("impossible input in "++ msg)

-- | stop the program if something is wrong
assert :: Bool -> String -> a -> a
assert cnd msg x = if cnd then x else error msg

warning :: Bool -> String -> a -> a
warning cnd msg x | cnd = trace ("WARNING: "++msg) x
                  | otherwise = x

infix 0  ///, //>

(///) :: Matrix Double -> String -> Matrix Double
m /// msg = debugMat msg 2 id m

(//>) :: Vector Double -> String -> Vector Double
v //> msg = debugMat msg 2 asRow v


trap :: Binary b => String -> (a -> Bool) -> b -> a -> a
trap msg goodcond y x =
    if not (goodcond x)
      then x
      else unsafePerformIO $ do
        encodeFile (msg++"_trapped.bin") y
        error "trap!"


