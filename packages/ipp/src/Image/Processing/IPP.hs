-----------------------------------------------------------------------------
{- |
Module      :  Image.Processing.IPP
Copyright   :  (c) Alberto Ruiz 2006-13
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  requires IPP

-}
-----------------------------------------------------------------------------

module Image.Processing.IPP (
    module Image.Processing.IPP.Pure,
    module Image.Processing.IPP.AdHoc,
    module Image.Processing.IPP.Structs,
    ippSetNumThreads
) where

import Image.Processing.IPP.Pure
import Image.Processing.IPP.AdHoc
import Image.Processing.IPP.Structs
import Image.Processing.IPP.Wrappers(ippSetNumThreads)

