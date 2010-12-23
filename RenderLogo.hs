-- Copyright © 2010 Bart Massey
-- Derived from material copyright The Open Group.
-- See the end of this file for license information
-- and original copyright notice.

-- Draw the "official" X Window System Logo, designed by
-- Danny Chong
-- 
-- Written by Ollie Jones, Apollo Computer
-- 
-- Does some fancy stuff to make the logo look acceptable
-- even if it is tiny.  Also makes the various linear
-- elements of the logo line up as well as possible
-- considering rasterization.
-- 
-- Munged to draw anti-aliased logo using Render extension.
-- Carl Worth, 2002-05-16
--
-- Reimplemented as Haskell. Bart Massey, 2010-11-11

module RenderLogo (toDrawable, defaultScreen,
                   LogoPixels(..), logoPixels, logoGC, 
                   renderLogoCore, renderLogoRender)
where

import Data.Bits
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Word
import Graphics.XHB
import Graphics.XHB.Gen.Render

import FixedBinary
import RenderUtils

logoRed :: (Word16, Word16, Word16)
logoRed = (0xd200, 0x2200, 0x3200)

logoGray :: (Word16, Word16, Word16)
logoGray = (0xd700, 0xd700, 0xd700)

toDrawable :: WINDOW -> DRAWABLE
toDrawable = fromXid .toXid

defaultScreen :: Connection -> SCREEN 
defaultScreen = head . roots_Setup . connectionSetup

colorInfo :: COLORMAP -> (Word16, Word16, Word16) -> AllocColor
colorInfo cm (r, g, b) = MkAllocColor {
  cmap_AllocColor = cm,
  red_AllocColor = r,
  green_AllocColor = g,
  blue_AllocColor = b }

data LogoPixels = LogoPixels {
  fgPixel :: Word32, 
  bgPixel :: Word32 }

-- Return foreground and background pixels for the logo colors.
logoPixels :: Connection -> IO LogoPixels
logoPixels c = do
  let cm = default_colormap_SCREEN $ defaultScreen c
  fgColorReceipt <- allocColor c $ colorInfo cm logoGray
  Right fgColor <- getReply fgColorReceipt
  let fg = pixel_AllocColorReply fgColor
  bgColorReceipt <- allocColor c $ colorInfo cm logoRed
  Right bgColor <- getReply bgColorReceipt
  let bg = pixel_AllocColorReply bgColor
  return $ LogoPixels fg bg

logoGC :: Connection -> WINDOW -> LogoPixels -> IO GCONTEXT
logoGC c w (LogoPixels fg bg) = do
  gc <- newResource c
  let gcValues = toValueParam [
        (GCForeground, fg),
        (GCBackground, bg)]
  let gcInfo = MkCreateGC {
        cid_CreateGC = gc,
        drawable_CreateGC = toDrawable w,
        value_CreateGC = gcValues }
  createGC c gcInfo
  return gc

-- This is the transliterated code from XLogo.
logoPolys :: Word16 -> Word16 -> [[Point Double]]
logoPolys width height =
  -- do a centered even-sized square, at least for now
  let isize = (width `min` height) .&. complement 1 in
  let x = fromIntegral $ (width - isize) `div` 2
      y = fromIntegral $ (height - isize) `div` 2
      size = fromIntegral isize in
  -- get some fundamental sizes
  let thin = size / 11.0
      thick = size / 4.0 in
  let gap = thin / 4.0 in
  let d31 = thin + thin + gap in
  -- set up the segments
  let thickLeft = 
        Line
         (Point x y)
         (Point (x + size - thick) (y + size)) :: Line Double
      thickRight = 
        Line
         (Point (x + thick) y)
         (Point (x + size) (y + size))
      thinLeft = 
        Line
         (Point (x + size - d31) y)
         (Point x (y + size))
      thinRight = 
        Line
         (Point (x + size) y)
         (Point (x + d31) (y + size))
      gapLeft = 
        Line
         (Point (x + size - (thin + gap)) y)
         (Point (x + thin) (y + size))
      gapRight = 
        Line
         (Point (x + size - thin) y)
         (Point (x + thin + gap) (y + size)) in
  let leftPoly = [ p1L thickLeft, 
                   p1L thickRight,
                   fromJust $ intersect thickRight gapLeft,
                   p2L gapLeft,
                   p2L thinLeft,
                   fromJust $ intersect thickLeft thinLeft ]
      rightPoly = [ p1L thinRight,
                    p1L gapRight,
                    fromJust $ intersect thickLeft gapRight,
                    p2L thickLeft,
                    p2L thickRight,
                    fromJust $ intersect thickRight thinRight ] in
  [leftPoly, rightPoly]

renderLogoCore :: Connection -> WINDOW -> GCONTEXT ->
                  Word16 -> Word16 -> IO ()
renderLogoCore c w gc width height = do
  clearArea c (MkClearArea False w 0 0 width height)
  mapM_ renderPoly $ logoPolys width height
  where
    renderPoly :: [Point Double] -> IO ()
    renderPoly poly = do
      let polyInfo = MkFillPoly {
            drawable_FillPoly = toDrawable w,
            gc_FillPoly = gc,
            shape_FillPoly = PolyShapeNonconvex,
            coordinate_mode_FillPoly = CoordModeOrigin,
            points_FillPoly = map fixupPoint poly }
            where
              fixupPoint :: Point Double -> POINT
              fixupPoint (Point x y) =
                MkPOINT (round x) (round y)
      fillPoly c polyInfo

renderLogoRender :: Connection -> WINDOW -> Word16 -> Word16 -> IO ()
renderLogoRender c w width height = do
  mapM_ (renderPoly . closePoly) $ logoPolys width height
  where
    renderPoly :: [Point Double] -> IO ()
    renderPoly poly = do
      pictureFormat <- findPictureFormat c w
      pictureId <- newResource c
      let pictureValue = emptyValueParam
      let picture = MkCreatePicture {
            pid_CreatePicture = pictureId,
            drawable_CreatePicture = toDrawable w,
            format_CreatePicture = pictureFormat,
            value_CreatePicture = pictureValue }
      createPicture c picture
      let traps = MkAddTraps {
            picture_AddTraps = pictureId,
            x_off_AddTraps = 0,
            y_off_AddTraps = 0,
            traps_AddTraps = map convertTrap $ polyTraps poly }
      addTraps c traps
      where
        convertTrap :: Trap Double -> TRAP
        convertTrap (Trap {
                        y1T = y1,
                        y2T = y2,
                        x11T = x11,
                        x12T = x12,
                        x21T = x21,
                        x22T = x22 }) =
          MkTRAP {
            top_TRAP = MkSPANFIX {
              l_SPANFIX = toFIXED x11,
              r_SPANFIX = toFIXED x12,
              y_SPANFIX = toFIXED y1 },
            bot_TRAP = MkSPANFIX {
              l_SPANFIX = toFIXED x21,
              r_SPANFIX = toFIXED x22,
              y_SPANFIX = toFIXED y2 }}
          where
            toFIXED :: Double -> FIXED
            toFIXED = 
              ff . fromRealFrac
              where
                ff :: B24_8 -> FIXED
                ff = fromFixed

-- showPoly :: String -> [Point Double] -> IO ()
-- showPoly name poly = do
--   putStr $ name ++ ": "
--   print $ map (\(Point x y) -> (x, y)) poly

findPictureFormat :: Connection -> WINDOW -> IO PICTFORMAT
findPictureFormat c w = do
  waReceipt <- getWindowAttributes c w
  Right wa <- getReply waReceipt
  let visual = visual_GetWindowAttributesReply wa
  pfReceipt <- queryPictFormats c
  Right pf <- getReply pfReceipt
  let pss = screens_QueryPictFormatsReply pf
  let pds = concatMap depths_PICTSCREEN pss
  let pvs = concatMap visuals_PICTDEPTH pds
  let Just pv = find ((visual ==) . visual_PICTVISUAL) pvs
  return $ format_PICTVISUAL pv


-- Copyright 1988, 1998  The Open Group
-- 
-- Permission to use, copy, modify, distribute, and sell this software and its
-- documentation for any purpose is hereby granted without fee, provided that
-- the above copyright notice appear in all copies and that both that
-- copyright notice and this permission notice appear in supporting
-- documentation.
-- 
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
-- OPEN GROUP BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
-- AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
-- CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-- 
-- Except as contained in this notice, the name of The Open Group shall not be
-- used in advertising or otherwise to promote the sale, use or other dealings
-- in this Software without prior written authorization from The Open Group.
