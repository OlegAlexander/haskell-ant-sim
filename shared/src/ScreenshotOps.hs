-- Functions for visual regression testing

module ScreenshotOps where

import Codec.Picture             -- JuicyPixels
import Control.Monad            (unless)

-- | Correct Raylib’s 2×-sized screenshot bug.
--   Loads the PNG, crops the lower-left quadrant,
--   and writes the fixed image *back to the same file*.
fixScreenshot :: FilePath -> IO ()
fixScreenshot path = do
  eitherImg <- readImage path
  dyn       <- case eitherImg of
                 Left err -> error ("fixScreenshot: " ++ err)
                 Right d  -> pure d

  let big   = convertRGBA8 dyn
      wBig  = imageWidth  big
      hBig  = imageHeight big
      w     = wBig `div` 2         -- expected real window width
      h     = hBig `div` 2         -- expected real window height

      -- lower-left crop: x ∈ [0..w-1], y ∈ [h..hBig-1]
      cropped = generateImage
                  (\x y -> pixelAt big x (y + h))
                  w h

  writePng path cropped            -- overwrite


-- | Compare two screenshots.
--   * True  -> images identical (no diff file written)
--   * False -> pixels differ   (diff PNG written)
--   * IOError -> input sizes differ
diffScreenshots :: FilePath      -- ^ first image
           -> FilePath      -- ^ second image
           -> FilePath      -- ^ where to write diff if they differ
           -> IO Bool
diffScreenshots fpA fpB fpOut = do
  imgA <- loadRGBA fpA
  imgB <- loadRGBA fpB

  -- Same dimensions?
  let w = imageWidth imgA
      h = imageHeight imgA
  unless (w == imageWidth imgB && h == imageHeight imgB) $
    ioError (userError "diffScreenshots: dimension mismatch")

  -- Fast equality check on raw pixel buffers
  if imageData imgA == imageData imgB
     then pure True                         -- identical; nothing to save
     else do
       -- Build a diff image: white where pixels differ
       let white = PixelRGBA8 255 255 255 255
           black = PixelRGBA8   0   0   0 255

           diffImg = generateImage
                        (\x y -> if pixelAt imgA x y == pixelAt imgB x y
                                    then black
                                    else white
                        ) w h
       writePng fpOut diffImg
       pure False

-- | Load an image and convert it to RGBA8 format.
loadRGBA :: FilePath -> IO (Image PixelRGBA8)
loadRGBA fp = do
  eDyn <- readImage fp
  case eDyn of
    Left err -> ioError (userError ("loadRGBA: " ++ err))
    Right d  -> pure (convertRGBA8 d)
