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

module RenderLogo (renderLogo)
where

import Data.Bits
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Word
import Graphics.XHB
import Graphics.XHB.Gen.Render

import FixedBinary
import RenderUtils

toDrawable :: WINDOW -> DRAWABLE
toDrawable = fromXid .toXid

renderLogo :: Connection -> WINDOW -> Word16 -> Word16 -> IO ()
renderLogo c w width height = do
  -- do a centered even-sized square, at least for now
  let isize = (width `min` height) .&. complement 1
  let x = fromIntegral $ (width - isize) `div` 2
  let y = fromIntegral $ (height - isize) `div` 2
  let size = (fromIntegral isize)
  -- get some fundamental sizes
  let thin = size / 11.0
  let thick = size / 4.0
  let gap = thin / 4.0
  let d31 = thin + thin + gap
  -- set up the segments
  let thickLeft = 
        Line
         (Point x y)
         (Point (x + size - thick) (y + size)) :: Line Double
  let thickRight = 
        Line
         (Point (x + thick) y)
         (Point (x + size) (y + size))
  let thinLeft = 
        Line
         (Point (x + size - d31) y)
         (Point x (y + size))
  let thinRight = 
        Line
         (Point (x + size) y)
         (Point (x + d31) (y + size))
  let gapLeft = 
        Line
         (Point (x + size - (thin + gap)) y)
         (Point thin (y + size))
  let gapRight = 
        Line
         (Point (x + size - thin) y)
         (Point (x + thin + gap) (y + size))
  -- left polygon
  renderPoly [ p1L thickLeft, 
               p1L thickRight,
               fromJust $ intersect thickRight gapLeft,
               p2L gapLeft,
               p2L thinLeft,
               fromJust $ intersect thickLeft thinLeft ]
  -- right polygon
  renderPoly [ p1L thinRight,
               p1L gapRight,
               fromJust $ intersect thickLeft gapRight,
               p2L thickLeft,
               p2L thickRight,
               fromJust $ intersect thickRight thinRight ]
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
