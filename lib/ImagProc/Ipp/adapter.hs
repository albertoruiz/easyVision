module Main where

import System.Environment
import Data.List
import Debug.Trace

debug x = trace (show x) x

getHeader ipp [header,name] = do
    f <- readFile (ipp++"/include/"++header)
    let raw = clean f
        thing = "IPPAPI(IppStatus,"++name++","
        found = filter (isPrefixOf thing) (tails raw)
        a = if null found then error $ name ++ " not found on "++header
                          else head found
        b = head $ filter (isSuffixOf "))") (inits a)
    return (prep.asList $ b)

clean = rep (") )","))") . rep (" (","(")  . rep ("( ","(") . rep (" ( ","(") . rep (", ",",")

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
    writeFile "Auto.hs" (autodefs hds)


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
ct "numThr" = "int"
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
ht "IppiMaskSize" = "CInt"
ht "IppCmpOp" = "CInt"
ht "IppHintAlgorithm" = "CInt"
ht "IppiAxis" = "CInt"
ht "int" = "Int"
ht "int*" = "Ptr CInt"
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
ht "numThr" = "CInt"
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

------------------------------------------------------------------

restArgs arity args = [(t,n)| (t,n) <- args, arity n]
mainArgs arity args = [(t,n)| (t,n) <- args, not (arity n)]

reorderArgs arity args = restArgs arity args ++ mainArgs arity args

pickArgs arity args = (restArgs arity args, mainArgs arity args)

arityaux 0 t = not $ t `elem` ["pDst", "dstStep", "DstStep","pDst[3]","dstStep[3]","roiSize","dstRoiSize"]

arityaux 1 t = arityaux 0 t && not (t `elem` ["pSrc", "srcStep","pSrc[3]","srcStep[3]"])

arityaux 2 t = arityaux 0 t && not (t `elem` ["pSrc1", "src1Step", "pSrc2", "src2Step","pSrc1[3]", "src1Step[3]", "pSrc2[3]", "src2Step[3]"])


hasDst tns = ("pDst" `elem` tns || "pDst[3]" `elem` tns) && ("roiSize" `elem` tns || "dstRoiSize" `elem` tns)
arity 2 tns = hasDst tns && ("pSrc2" `elem` tns || "pSrc2[3]" `elem` tns)
arity 1 tns = hasDst tns && ("pSrc" `elem` tns || "pSrc[3]" `elem` tns)
arity 0 tns = hasDst tns && not (arity 2 tns) && not (arity 1 tns)


autofun k hds = unlines [ver k f | f@(_, args) <- hds, arity k (map snd args)]

autodefs hds = (automod++) $ rep ("DstStep","dstStep") $ rep ("[3]","") $ rep ("(f )","f") $
    "\n-------- arity 0 -------------\n\n" ++
    autofun 0 hds
    ++ "-------- arity 1 -------------\n\n" ++
    autofun 1 hds
    ++ "\n------ arity 2 -------------\n\n" ++
    autofun 2 hds
    ++ "\n----------------------------\n"

ver k (n,args) = "io"++drop 4 n ++" "++ par ++ " = " ++ mk ++ "\n    where " ++
                     "f " ++ unwords (map tr args') ++ " = " ++ n ++" "++ unwords (map tr args) ++ "\n"
    where args' = reorderArgs ari args
          tr ("IppiSize",n) = n++"_w "++n++"_h"
          tr ("IppiPoint",n) = n++"_x "++n++"_y"
          tr (_,n) = n
          ari = arityaux k
          par = unwords (map tr (restArgs ari args))
          mk = autoname++" ("++unwords ["f",par]++")"
          autoname = "auto_"++show k++"_" ++(suffix n) where

suffix n = (iterate (tail.dropWhile (/='_')) n) !! k
    where k = length (filter (== '_') n) - 1

automod  = "-- generated automatically by adapter.hs\n\n"
     ++ "{-# OPTIONS #-}\n\n"
     ++ "module ImagProc.Ipp.Auto where\n\n"
--     ++ "import Foreign\nimport Foreign.C.Types\n"
     ++ "import ImagProc.Ipp.AutoGen\nimport ImagProc.Ipp.Adapt\n"
