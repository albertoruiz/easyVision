-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Camera
Copyright   :  (c) Alberto Ruiz 2006-12
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)

Video sources

-}
-----------------------------------------------------------------------------

module ImagProc.Camera(
    -- * Camera selection
    findSize,
    getCam, numCams,
    ImagProc.Camera.getCams, ImagProc.Camera.getMulticam,
    readFrames,
    ImagProc.Camera.readImages, ImagProc.Camera.readFolderMP, ImagProc.Camera.readFolderIM,
    -- * Video I/O
    writeFrames,
    optionalSaver,
    autoSaver,
    process,
    saveFrame,
    module ImagProc.Camera.MPlayer
)where

import Image.Camera as IC
import Image.Core
import Image.Base

import ImagProc.Camera.MPlayer
import ImagProc.Generic(Channels,channels,GImg,toYUV,channelsFromRGB)
import Control.Applicative((<$>))
import Control.Arrow((***))
import Util.LazyIO((>~>),lazyList,Generator)
import Util.Options(optionString)



getCams :: IO [IO (Maybe Channels)]
getCams = (((channels <$>) <$>) <$>) <$> IC.getCams


getMulticam :: Size -> Int -> Generator [Channels]
getMulticam sz n = do
    cams <- mapM (flip getCam sz >~> channels) [0..n-1]
    return (fmap sequence $ sequence cams)


-- | offline video processing. The input is the camera 0 and the output
--   is given by --save=[saved.yuv]
process :: (GImg pixel a) => ([Channels] -> [a]) -> IO ()
process f = do
    outfile <- optionString "--save" "saved.yuv"
    xs <- readFrames 0
    let ys = map toYUV . f . map channels $ xs
    writeFrames outfile ys
 

readFolderIM :: FilePath -> IO [(Channels,String)]
-- ^ reads a list of images from a folder. Variable size, using imageMagick
readFolderIM path = map (channelsFromRGB *** id) <$> IC.readFolderIM path


readImages :: [FilePath] -> IO [Channels]
-- ^ reads a list of images (variable size, using imageMagick)
readImages fs = map channelsFromRGB <$> IC.readImages fs


readFolderMP :: FilePath -> Maybe Size -> IO [(Channels,String)]
-- ^ reads a list of images from a folder. Fixed size using mplayer
readFolderMP path mbsz = map (channels *** id) <$> IC.readFolderMP path mbsz


