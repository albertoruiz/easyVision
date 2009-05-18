{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.MiniApps.SiftParams
Copyright   :  (c) Alberto Ruiz 2009
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)

-}
-----------------------------------------------------------------------------

module EasyVision.MiniApps.SiftParams (
    userSIFTParams
) where

import ImagProc.C.SIFT(SIFTParams(..), defaultSIFTParams)
import Control.Applicative
import EasyVision.Util
import EasyVision.Parameters

userSIFTParams = do
    SIFTParams{..} <- getSIFTParams

    o <- createParameters' "SIFT Parameters"
        [ ("oct1" , intParam    oct1   0 3)
        , ("thres", realParam   thres  0 0.01)
        , ("nmax",  intParam    nmax   0 2000)
        ]

    return $ SIFTParams <$> getParam o "oct1"
                        <*> getParam o "thres"
                        <*> getParam o "nmax"

getSIFTParams = SIFTParams <$> getOption "--oct1"  oct1
                           <*> getOption "--thres" thres
                           <*> getOption "--nmax"  nmax

    where SIFTParams{..} = defaultSIFTParams
