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

import Data.Word
import Graphics.XHB
import Graphics.XHB.Gen.Render

import RenderUtils

renderLogo :: Connection -> Int -> PICTURE -> PICTURE -> PICTFORMINFO
                         -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
renderLogo c op src dst maskFormat x y width height = do
  -- do a centered even-sized square, at least for now
  let size = (width `min` height) .&. complement 1
  let x' = (width - size) `div` 2
  let y' = (height - size) `div` 2
  -- get some fundamental sizes
  let thin = fromIntegral size / 11.0
  let thick = fromIntegral size / 4.0
  let gap = thin / 4.0
  let d31 = thin + thin + gap
  -- set up the segments
  let thickLeft = 
        MkLine
         (MkPoint x y)
         (MkPoint (x + size - thick) (y + size))
  let thickRight = 
        MkLine
         (MkPoint (x + thick) y)
         (MkPoint (x + size) (y + size))
  let thinLeft = 
        MkLine
         (MkPoint (x + size - d31) y)
         (MkPoint x (y + size))
  let thinRight = 
        MkLine
         (MkPoint (x + size) y)
         (MkPoint (x + d31) (y + size))
  let gapLeft = 
        MkLine
         (MkPoint (x + size - (thin + gap)) y)
         (MkPoint thin (y + size))
  let gapRight = 
        MkLine
         (MkPoint (x + size - thin) y)
         (MkPoint (x + thin + gap) (y + size))
  -- left polygon
  renderPoly [ p1 thickLeft, 
               p1 thickRight,
               fromJust $ intersect thickRight gapLeft,
               p2 gapLeft,
               p2 thinLeft,
               fromJust $ intersect thickLeft thinLeft ]
  -- right polygon
  renderPoly [ p1 thinRight,
               p1 gapRight,
               fromJust $ intersect thickLeft gapRight,
               p2 thickLeft,
               p2 thickRight,
               fromJust $ intersect thickRight thinRight ]
  where
    renderPoly :: [Double] -> IO ()
    renderPoly p =
      renderCompositePoly c op src dst maskFormat 0 0 0 0 p 0

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
