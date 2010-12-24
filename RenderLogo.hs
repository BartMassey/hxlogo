-- Copyright © 2010 Bart Massey
-- Some code derived from material copyright The Open Group.
-- See the end of this file for original license information
-- and copyright notice. 
-- 
-- All non-TOG code in this file is licensed under the
-- "3-clause ('new') BSD License" as specified elsewhere in
-- this distribution.  See the file COPYING in this
-- distribution for the believed-compatible license for the
-- remainder of this file.
-- 
-- Draw the "official" X Window System Logo, designed by
-- Danny Chong
-- 
-- Original C written by Ollie Jones, Apollo Computer
-- 
-- Does some fancy stuff to make the logo look acceptable
-- even if it is tiny.  Also makes the various linear
-- elements of the logo line up as well as possible
-- considering rasterization.
-- 
-- C munged to draw anti-aliased logo using Render extension.
-- Carl Worth, 2002-05-16
--
-- Reimplemented as Haskell. Bart Massey, 2010-11-11

module RenderLogo (LogoPixels(..), logoPixels, logoGC, renderLogoCore,
                   logoWindowPicture, logoGrayPicture, renderLogoRender)
where

import Data.Bits
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Word
import Graphics.XHB
import Graphics.XHB.Gen.Render

import Graphics.XHB.Utils
import Data.FixedBinary
import Graphics.PolyUtils

logoRed :: (Word16, Word16, Word16)
logoRed = (0xd200, 0x2200, 0x3200)

logoGray :: (Word16, Word16, Word16)
logoGray = (0xd700, 0xd700, 0xd700)

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

logoCreatePicture :: Connection -> DRAWABLE -> PICTFORMAT -> IO PICTURE
logoCreatePicture c drawable pictureFormat = do
  pictureId <- newResource c
  let pictureValue = toValueParam [(CPRepeat, toValue RepeatNormal)]
  let picture = MkCreatePicture {
        pid_CreatePicture = pictureId,
        drawable_CreatePicture = drawable,
        format_CreatePicture = pictureFormat,
        value_CreatePicture = pictureValue }
  createPicture c picture
  return pictureId

logoCOLOR :: (Word16, Word16, Word16) -> COLOR
logoCOLOR (r, g, b) =
  MkCOLOR {
    red_COLOR = r,
    green_COLOR = g,
    blue_COLOR = b,
    alpha_COLOR = 0xffff }

logoGrayPicture :: Connection -> WINDOW -> IO PICTURE
logoGrayPicture c w = do
  let depth = root_depth_SCREEN $ defaultScreen c
  pixmap <- newResource c
  createPixmap c $ MkCreatePixmap {
    depth_CreatePixmap = depth,
    pid_CreatePixmap = pixmap,
    drawable_CreatePixmap = toDrawable w,
    width_CreatePixmap = 1,
    height_CreatePixmap = 1 }
  pictureFormat <- findWindowPictureFormat c w
  pictureId <- logoCreatePicture c (toDrawable pixmap) pictureFormat
  let rect = MkRECTANGLE {
        x_RECTANGLE = 0,
        y_RECTANGLE = 0,
        width_RECTANGLE = 1,
        height_RECTANGLE = 1 }
  fillRectangles c $ MkFillRectangles {
        op_FillRectangles = PictOpAdd,
        dst_FillRectangles = pictureId,
        color_FillRectangles = logoCOLOR logoGray,
        rects_FillRectangles = [rect] }
  return pictureId
                             
logoWindowPicture :: Connection -> WINDOW -> IO PICTURE
logoWindowPicture c w = do
  pictureFormat <- findWindowPictureFormat c w
  logoCreatePicture c (toDrawable w) pictureFormat

renderLogoRender :: Connection -> PICTURE -> PICTURE ->
                    Word16 -> Word16 -> IO ()
renderLogoRender c grayPicture windowPicture width height = do
  let trapPolys = 
        map convertTrap $ concatMap (polyTraps . closePoly)
                        $ logoPolys width height
  formatA8 <- findStandardPictureFormat c PictureFormatA8
  let traps = MkTrapezoids {
        op_Trapezoids = PictOpOver,
        src_Trapezoids = grayPicture,
        dst_Trapezoids = windowPicture,
        mask_format_Trapezoids = formatA8,
        src_x_Trapezoids = 0,
        src_y_Trapezoids = 0,
        traps_Trapezoids = trapPolys }
  trapezoids c traps
  where
    convertTrap :: Trap Double -> TRAPEZOID
    convertTrap (Trap {
                    y1T = y1,
                    y2T = y2,
                    x11T = x11,
                    x12T = x12,
                    x21T = x21,
                    x22T = x22 }) =
      MkTRAPEZOID {
        top_TRAPEZOID = toFIXED y1,
        bottom_TRAPEZOID = toFIXED y2,
        left_TRAPEZOID = MkLINEFIX {
          p1_LINEFIX = MkPOINTFIX {
             x_POINTFIX = toFIXED x11,
             y_POINTFIX = toFIXED y1 },
          p2_LINEFIX = MkPOINTFIX {
             x_POINTFIX = toFIXED x21,
             y_POINTFIX = toFIXED y2 }},
        right_TRAPEZOID = MkLINEFIX {
          p1_LINEFIX = MkPOINTFIX {
             x_POINTFIX = toFIXED x12,
             y_POINTFIX = toFIXED y1 },
          p2_LINEFIX = MkPOINTFIX {
             x_POINTFIX = toFIXED x22,
             y_POINTFIX = toFIXED y2 }}}
      where
        toFIXED :: Double -> FIXED
        toFIXED d = 
          fromFixed (fromRealFrac d :: B16_16)

-- showPoly :: String -> [Point Double] -> IO ()
-- showPoly name poly = do
--   putStr $ name ++ ": "
--   print $ map (\(Point x y) -> (x, y)) poly

data StandardPictureFormat = 
  PictureFormatARGB32 |
  PictureFormatRGB24 |
  PictureFormatA8 |
  PictureFormatA4 |
  PictureFormatA1

-- Returns (depth, alpha-bits, rgb-bits)
pictFormatInfo :: StandardPictureFormat -> (Word8, Word16, Word16)
pictFormatInfo PictureFormatARGB32 = (32, 8, 8)
pictFormatInfo PictureFormatRGB24 = (24, 0, 8)
pictFormatInfo PictureFormatA8 = (8, 8, 0)
pictFormatInfo PictureFormatA4 = (4, 4, 0)
pictFormatInfo PictureFormatA1 = (1, 1, 0)

-- XXX The component order isn't checked, so could return some
-- freakish format.
findStandardPictureFormat :: Connection -> 
                             StandardPictureFormat -> IO PICTFORMAT
findStandardPictureFormat c stdFmt = do
  pfReceipt <- queryPictFormats c
  Right pf <- getReply pfReceipt
  let fmts = formats_QueryPictFormatsReply pf
  let Just sf = find correctFmt fmts
  return $ id_PICTFORMINFO sf
  where
    correctFmt :: PICTFORMINFO -> Bool
    correctFmt fmt =
      let (depth, a, rgb) = pictFormatInfo stdFmt in
      depth_PICTFORMINFO fmt == depth &&
      case type_PICTFORMINFO fmt of
        PictTypeDirect ->
          let d = direct_PICTFORMINFO fmt 
              aMask = (1 `shiftL` fromIntegral a) - 1
              rgbMask = (1 `shiftL` fromIntegral rgb) - 1 in
          red_mask_DIRECTFORMAT d == rgbMask &&
          green_mask_DIRECTFORMAT d == rgbMask &&
          blue_mask_DIRECTFORMAT d == rgbMask &&
          alpha_mask_DIRECTFORMAT d == aMask
        _ -> False
      
findWindowPictureFormat :: Connection -> WINDOW -> IO PICTFORMAT
findWindowPictureFormat c w = do
  waReceipt <- getWindowAttributes c w
  Right wa <- getReply waReceipt
  let visual = visual_GetWindowAttributesReply wa
  findPictureFormat c visual
  
findPictureFormat :: Connection -> VISUALID -> IO PICTFORMAT
findPictureFormat c visual = do
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
