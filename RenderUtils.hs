-- Copyright © 2010 Bart Massey
-- Some code derived from material copyright The Open Group.
-- See the end of this file for license information
-- and original copyright notice.

module RenderUtils (Point(..), Line(..), 
                    x1L, y1L, x2L, y2L,
                    invSlope, xIntercept, intersection)
                    
where

data Point a = MkPoint { xP :: a, yP :: a }

data Line a = MkLine { p1L :: Point a, p2L :: Point a }

x1L :: Line a -> a
x1L = xP . p1L

y1L :: Line a -> a
y1L = yP . p1L

x2L :: Line a -> a
x2L = xP . p2L

y2L :: Line a -> a
y2L = yP . p2L

-- The rest of this file is TOG-derived

invSlope :: RealFrac a => Line a -> a
invSlope l = (x2L l - x1L l) / (y2L l - y1L l)

xIntercept :: RealFrac a => Line a -> a -> a
xIntercept l invSlope = x1L l - invSlope * y1L l
                     
intersection :: RealFrac a => Line a -> Line a -> Point a
intersection l1 l2 = 
  -- x = m1y + b1
  -- x = m2y + b2
  -- m1y + b1 = m2y + b2
  -- y * (m1 - m2) = b2 - b1
  -- y = (b2 - b1) / (m1 - m2)
  let m1 = invSlope l1
      b1 = xIntercept l1 m1
      m2 = invSlope l2
      b2 = xIntercept l2 m2
      y = (b2 - b1) / (m1 - m2) in
  MkPoint {
    xP = m1 * y + b1,
    yP = y }

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
