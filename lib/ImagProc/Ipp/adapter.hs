import System.Environment
import Data.List

getHeader ipp [header,name] = do
    f <- readFile (ipp++"/include/"++header)
    let raw = clean f
        thing = "IPPAPI(IppStatus,"++name++","
        found = filter (isPrefixOf thing) (tails raw)
        a = if null found then error $ name ++ " not found on "++header
                          else head found
        b = head $ filter (isSuffixOf "))") (inits a)
    return (prep.asList $ b)

clean = rep (" (","(")  . rep ("( ","(") . rep (" ( ","(") . rep (", ",",")

rep (c,r) [] = []
rep (c,r) f@(x:xs) 
  | c `isPrefixOf` f = r ++ rep (c,r) (drop (length c) f)
  | otherwise        = x:(rep (c,r) xs)

asList = words . rep ("\n"," ") . rep ("("," ") . rep (")"," ") . rep (","," ") . rep ("const"," ")

prep hl = (name,args) where
    name = hl!!2
    args = p2 $ drop 3 hl

p2 [] = []
p2 (a:b:rest) = (a,b) : p2 rest

main = do
    ipp <- getEnv "IPP"
    f <- readFile "functions.txt"
    let names = map words (lines f)
    hds <- mapM (getHeader ipp) names
    mapM_ (putStrLn.fst) hds
    writeFile "adapt.h" (chead ++ (unlines $ map mkh hds))
    writeFile "adapt.c" (cdef ++ (unlines $ map mkd hds))
    writeFile "Adapt.hs" (wmod ++ (unlines $ map mkw hds))


mkh' (n,as) = "int "++n ++"x(" ++ concat (intersperse ", " (map tr' as))++")"

tr' ("IppiSize",n) = "int "++n++"_w, "++"int "++n++"_h"
tr' ("IppiPoint",n) = "int "++n++"_x, "++"int "++n++"_y"
tr' (t,n) = t++" "++n


mkh (n,as) = "int "++n ++"x(" ++ concat (intersperse ", " (map tr as))++");"


tr (t,n) = ct (t ++ isPointer n)

ct "IppiSize" = "int, int"
ct "IppiPoint" = "int, int"
ct "IppiMaskSize" = "int"
ct "IppCmpOp" = "int"
ct "IppHintAlgorithm" = "int"
ct "IppiAxis" = "int"
ct "int" = "int"
ct "int*" = "int*"
ct "float" = "float"
ct "float*" = "float*"
ct "double" = "double"
ct "double*" = "double*"
ct "Ipp8u" = "unsigned char"
ct "Ipp8u*" = "unsigned char*"
ct "Ipp8u**" = "void*"
ct "Ipp32f" = "float"
ct "Ipp32f*" = "float*"
ct "Ipp64f" = "double"
ct "Ipp64f*" = "double*"
ct "Ipp32s*" = "int*"
ct "IppiFFTSpec_R_32f**" = "void*"
ct "IppiFFTSpec_R_32f*" = "void*"
ct "IppiConnectedComp*" = "void*"
ct x = error $ " UNKNOWN TYPE: "++x



-------------------------------------------------------

mkd (n,as)  = mkh' (n,as) ++ " {\n" ++
              auxSizes as ++
              auxPoints as ++
              "    return "++n++"(" ++ concat (intersperse ", " (map cl as))++");\n}\n"


auxSizes as = unlines $ map (initSize.snd) $ filter (\(t,n)->t=="IppiSize") as

initSize n = "    IppiSize "++n++" = {"++n++"_w,"++n++"_h};"

auxPoints as = unlines $ map (initPoint.snd) $ filter (\(t,n)->t=="IppiPoint") as

initPoint n = "    IppiPoint "++n++" = {"++n++"_x,"++n++"_y};"


cl (t,n) = takeWhile (/= '[') n

-------------------------------------------------------

mkw (n,as) = "foreign import ccall \"adapt.h "++n++"x\"\n    "++n++" :: "
             ++ concat (intersperse " -> " (map wt as))++" -> IO Int\n"

wt (t,n) = ht (t ++ isPointer n)

isPointer n = if last n == ']' then "*" else ""

ht "IppiSize" = "Int -> Int"
ht "IppiPoint" = "Int -> Int"
ht "IppiMaskSize" = "Int"
ht "IppCmpOp" = "Int"
ht "IppHintAlgorithm" = "Int"
ht "IppiAxis" = "Int"
ht "int" = "Int"
ht "int*" = "Ptr Int"
ht "float" = "Float"
ht "float*" = "Ptr Float"
ht "double" = "Double"
ht "double*" = "Ptr Double"
ht "Ipp8u" = "CUChar"
ht "Ipp8u*" = "Ptr CUChar"
ht "Ipp8u**" = "Ptr ()"
ht "Ipp32f" = "Float"
ht "Ipp32f*" = "Ptr Float"
ht "Ipp64f" = "Double"
ht "Ipp64f*" = "Ptr Double"
ht "Ipp32s*" = "Ptr Int32"
ht "IppiFFTSpec_R_32f*" = "Ptr ()"
ht "IppiFFTSpec_R_32f**" = "Ptr ()"
ht "IppiConnectedComp*" = "Ptr IppiConnectedComp"
ht x = error $ " UNKNOWN TYPE: "++x

-----------------------------------------------------------------

wmod  = "-- generated automatically by adapter.hs\n\n"
     ++ "{-# OPTIONS -ffi -fvia-C #-}\n\n"
     ++ "module ImagProc.Ipp.Adapt where\n\n"
     ++ "import Foreign\nimport Foreign.C.Types\n"
     ++ "import ImagProc.Ipp.Structs\n\n"

chead = "/* generated automatically by adapter.hs */\n\n"

cdef  = "/* generated automatically by adapter.hs */\n\n"
     ++ "#include <ipp.h>\n\n"
