module Main where

import System.Environment
import Data.List
import Debug.Trace
import Parser
import Control.Monad

debug x = trace (show x) x

rep (c,r) [] = []
rep (c,r) f@(x:xs) 
  | c `isPrefixOf` f = r ++ rep (c,r) (drop (length c) f)
  | otherwise        = x:(rep (c,r) xs)

noconst h = h { args = map f (args h) }
    where f (ts,n) = (filter (/="const") ts, n)

-- convert the parsed header structure to the most simple tuple used before
toTuple h = (name h, map f (args h))
    where f ([t],n) = (t,n)

main = do
    ipp <- getEnv "IPP_INC"
    f <- readFile "functions.txt"
    let hdsnames = filter (not . null) . map words . lines $ f
        names = map last hdsnames
        headers = map ((ipp++"/")++) $ nub (map head hdsnames)
    hs <- fmap (map noconst . filter ((`elem` names) . name) . concatMap getHeaders) (mapM readFile headers)
    mapM_ (print.name) hs

    let hds = map toTuple hs
    writeFile "ptr_adapt.h" (chead ++ (unlines $ map mkh hds))
    writeFile "ptr_adapt.c" (cdef ++ (unlines $ map mkd hds))
    writeFile "Adapt.hs" (wmod ++ (unlines $ map mkw hds))
    writeFile "Auto.hs" (autodefs hs)


mkh' (n,as) = "int "++n ++"x(" ++ concat (intersperse ", " (map tr' as))++")"

tr' ("IppiRect",n) = "IppiRect* "++n
tr' ("IppiSize",n) = "IppiSize* "++n
tr' ("IppiPoint",n) =  "IppiPoint* "++n
tr' ("Ipp8u*","pSrc[3]") = "const Ipp8u* const pSrc[3]"
tr' (t,n) = t++" "++n


mkh (n,as) = "int "++n ++"x(" ++ concat (intersperse ", " (map tr as))++");"


tr (t,n) = ct (t ++ isPointer n)

ct "IppiRect" = "void*"
ct "IppiSize" = "void*"
ct "IppiPoint" = "void*"
ct "IppiMaskSize" = "int"
ct "IppiNorm" = "int"
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
ct "Ipp8u**" = "void**"
ct "Ipp32f" = "float"
ct "Ipp32f*" = "float*"
ct "Ipp64f" = "double"
ct "Ipp64f*" = "double*"
ct "Ipp32s*" = "int*"
ct "IppiFFTSpec_R_32f**" = "void**"
ct "IppiFFTSpec_R_32f*" = "void*"
ct "IppiConnectedComp*" = "void*"
ct "numThr" = "int"
ct x = error $ " UNKNOWN TYPE: "++x



-------------------------------------------------------

mkd (n,as)  = mkh' (n,as) ++ " {\n" ++
              "    return "++n++"(" ++ concat (intersperse ", " (map cl as))++");\n}\n"

cl ("IppiRect" , n) = '*':n
cl ("IppiSize" , n) = '*':n
cl ("IppiPoint", n) = '*':n
cl (_,n) = takeWhile (/= '[') n

-------------------------------------------------------

mkw (n,as) = rep ("DstStep","dstStep")$ 
             "foreign import ccall \""++n++"x\"\n    "++n++"x :: "
             ++ concat (intersperse " -> " (map wt as))++" -> IO CInt\n"
             ++ n ++" " ++ unwords (map cl' as) ++ " = do\n"
             ++ auxStruct initStruct as
             ++ "    r <- "++n++"x " ++ unwords (map cl'' as)++"\n"
             ++ auxStruct freeStruct as
             ++ "    return r\n"

auxStruct act args = unlines $ map (act.snd) $ filter (\(t,n)->t=="IppiSize"||t=="IppiPoint"||t=="IppiRect") args
initStruct n = "    p"++n++" <- new "++n
freeStruct n = "    free p"++n

--cl ("IppiSize" , n) = '*':n
--cl ("IppiPoint", n) = '*':n
cl' (_,n) = takeWhile (/= '[') n

cl'' ("IppiSize" , n) = 'p':n
cl'' ("IppiPoint", n) = 'p':n
cl'' (_,n) = takeWhile (/= '[') n


wt (t,n) = ht (t ++ isPointer n)

isPointer n = if last n == ']' then "*" else ""

ht "IppiRect" = "Ptr IppiRect"
ht "IppiSize" = "Ptr IppiSize"
ht "IppiPoint" = "Ptr IppiPoint"
ht "IppiNorm" = "CInt"
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
ht "Ipp8u" = "Word8"
ht "Ipp8u*" = "Ptr Word8"
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
     ++ "{-# LANGUAGE ForeignFunctionInterface #-}\n"
     ++ "{-# OPTIONS #-}\n\n"
     ++ "module ImagProc.Ipp.Adapt where\n\n"
     ++ "import Foreign\nimport Foreign.C.Types\n"
     ++ "import ImagProc.Ipp.Structs\n\n"

chead = "/* generated automatically by adapter.hs */\n\n"

cdef  = "/* generated automatically by adapter.hs */\n\n"
     ++ "#include <ipp.h>\n\n"

------------------------------------------------------------------

restArgs ar args = [(t,n)| (t,n) <- args, ar n]
mainArgs ar args = [(t,n)| (t,n) <- args, not (ar n)]

reorderArgs ar args = restArgs ar args ++ mainArgs ar args

pickArgs ar args = (restArgs ar args, mainArgs ar args)

arityaux 0 t = not $ t `elem` ["pDst", "dstStep", "DstStep","pDst[3]","dstStep[3]","roiSize","dstRoiSize"]

arityaux 1 t = arityaux 0 t && not (t `elem` ["pSrc", "srcStep","pSrc[3]","srcStep[3]", "pSqr", "sqrStep"])

arityaux 2 t = arityaux 0 t && not (t `elem` ["pSrc1", "src1Step", "pSrc2", "src2Step","pSrc1[3]",
                                              "src1Step[3]", "pSrc2[3]", "src2Step[3]",
                                              "pSrc", "srcStep","pSqr","sqrStep"])

arityaux 11 t = not (t `elem` ["pSrc","srcStep","pSrcDst","srcDstStep","roiSize"])


-- arity is the general detector, while arityaux is the detector of additional arguments
hasDst tns = ("pDst" `elem` tns || "pDst[3]" `elem` tns)
           && ("roiSize" `elem` tns || "dstRoiSize" `elem` tns)
           && ("dstStep" `elem` tns)
arity 2 tns = hasDst tns && ("pSrc2" `elem` tns || "pSrc2[3]" `elem` tns || special tns)
arity 1 tns = hasDst tns && ("pSrc" `elem` tns || "pSrc[3]" `elem` tns) && not (special tns)
arity 0 tns = hasDst tns && not (arity 2 tns) && not (arity 1 tns)
arity 11 tns = "pSrcDst" `elem` tns -- inplace


special tns = "pSrc srcStep pSqr sqrStep pDst dstStep roiSize rect" == unwords tns

--autofun k hds = unlines [ver k f | f@(_, args) <- hds, arity k (map snd args)]
autofun k hds = unlines [ver k (toTuple h) (doc h) | h <- hds, arity k (map snd (args h))]

autodefs hds = (automod++) $ rep ("srcDstXStep","srcDstStep")
                           $ rep ("DstStep","dstStep") $ rep ("srcDstStep","srcDstXStep")
                           $ rep ("[3]","") $ rep ("[3][4]","") $ rep ("[]","") $ rep ("(f )","f") $
    "\n-------- arity 0 -------------\n\n" ++
    autofun 0 hds
    ++ "-------- arity 1 -------------\n\n" ++
    autofun 1 hds
    ++ "\n------ arity 2 -------------\n\n" ++
    autofun 2 hds
    ++ "\n------ inplace arity 2 ------\n\n" ++
    autofun 11 hds
    ++ "\n----------------------------\n"

ver k (n,args) doc = mkdoc ++ "io"++drop 4 n ++" "++ par ++ " = " ++ scc ++ mk ++ "\n    where " ++
                     "f " ++ unwords (map tr args') ++ " = " ++ n ++" "++ unwords (map tr args) ++ "\n"
    where args' = reorderArgs ari args
          --tr ("IppiSize",n) = n++"_w "++n++"_h"
          --tr ("IppiPoint",n) = n++"_x "++n++"_y"
          tr (_,n) = n
          ari = arityaux k
          par = unwords (map tr (restArgs ari args))
          mk = autoname++" ("++unwords ["f",par]++") "++"\""++n++"\""
          autoname = "auto_"++show k++"_" ++(suffix n) where
          scc = "{-# SCC \""++n++"\" #-} "
          mkdoc = "{- | " ++ doc ++ " -}\n"

suffix n = (iterate (tail.dropWhile (/='_')) n) !! k
    where k = length (filter (== '_') n) - 1

automod  = "-- generated automatically by adapter.hs\n\n"
     ++ "{-# OPTIONS #-}\n\n"
     ++ "module ImagProc.Ipp.Auto where\n\n"
     ++ "import ImagProc.Ipp.AutoGen\nimport ImagProc.Ipp.Adapt\n"
