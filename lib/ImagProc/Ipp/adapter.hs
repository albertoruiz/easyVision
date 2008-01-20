import System.Process
import Data.List

ipp = "/opt/intel/ipp/5.3/ia32/include/"

getHeader [header,name] = do
    f <- readFile (ipp++header)
    let raw = rep (", ",",") f
        thing = "IPPAPI(IppStatus,"++name++","
        a = head $ filter (isPrefixOf thing) (tails raw)
        b = head $ filter (isSuffixOf "))") (inits a)
    return (prep.asList $ b)

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
    f <- readFile "functions.txt"
    let names = map words (lines f)
    hds <- mapM getHeader names
    writeFile "adapt.h" (chead ++ (unlines $ map mkh hds))
    writeFile "adapt.c" (cdef ++ (unlines $ map mkd hds))
    writeFile "Adapt.hs" (wmod ++ (unlines $ map mkw hds))


mkh' (n,as) = "int "++n ++"x(" ++ concat (intersperse ", " (map tr' as))++")"

tr' ("IppiSize",n) = "int "++n++"_w, "++"int "++n++"_h"
tr' (t,n) = t++" "++n


mkh (n,as) = "int "++n ++"x(" ++ concat (intersperse ", " (map tr as))++");"


tr (t,n) = ct (t ++ isPointer n)

ct "IppiSize" = "int, int"
ct "int" = "int"
ct "int*" = "int*"
ct "float" = "float"
ct "float*" = "float*"
ct "double" = "double"
ct "double*" = "double*"
ct "Ipp8u" = "unsigned char"
ct "Ipp8u*" = "void*"
ct "Ipp8u**" = "void*"
ct "Ipp32f" = "float"
ct "Ipp32f*" = "void*"
ct x = "**** "++x



-------------------------------------------------------

mkd (n,as)  = mkh' (n,as) ++ " {\n" ++
              auxSizes as ++
              "    return "++n++"(" ++ concat (intersperse ", " (map cl as))++");\n}\n"


auxSizes as = unlines $ map (initSize.snd) $ filter (\(t,n)->t=="IppiSize") as

initSize n = "    IppiSize "++n++" = {"++n++"_w,"++n++"_h};"

cl (t,n) = takeWhile (/= '[') n

-------------------------------------------------------

mkw (n,as) = "foreign import ccall \"adapt.h "++n++"x\"\n    "++n++" :: "
             ++ concat (intersperse " -> " (map wt as))++" -> IO Int\n"

wt (t,n) = ht (t ++ isPointer n)

isPointer n = if last n == ']' then "*" else ""

ht "IppiSize" = "Int -> Int"
ht "int" = "Int"
ht "int*" = "Ptr Int"
ht "float" = "Float"
ht "float*" = "Ptr Float"
ht "double" = "Double"
ht "double*" = "Ptr Double"
ht "Ipp8u" = "CUChar"
ht "Ipp8u*" = "Ptr ()"
ht "Ipp8u**" = "Ptr ()"
ht "Ipp32f" = "Float"
ht "Ipp32f*" = "Ptr ()"
ht x = "**** "++x

-----------------------------------------------------------------

wmod  = "-- generated automatically by adapter.hs\n\n"
     ++ "{-# OPTIONS -ffi #-}\n\n"
     ++ "module ImagProc.Ipp.Adapt where\n\n"
     ++ "import Foreign\n\n"

chead = "/* generated automatically by adapter.hs */\n\n"

cdef  = "/* generated automatically by adapter.hs */\n\n"
     ++ "#include <ipp.h>\n\n"
