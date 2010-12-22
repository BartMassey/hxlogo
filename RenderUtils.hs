-- Copyright © 2010 Bart Massey
-- Some code derived from material copyright The Open Group.
-- See the end of this file for license information
-- and original copyright notice.

module RenderUtils (Point(..), Line(..), 
                    x1L, y1L, x2L, y2L,
                    invSlope, xIntercept, intersect)
where
  
import Data.List (sort)  

data RealFrac a => Point a = Point { xP :: a, yP :: a } deriving Eq

instance RealFrac a => Ord (Point a) where
  p1 `compare` p2 = (yP p1, xP p1) `compare` (yP p2, xP p2)

data RealFrac a => Line a = Line { p1L :: Point a, p2L :: Point a } deriving Eq

instance RealFrac a => Ord (Line a) where
  l1 `compare` l2 = (p1L l1, p2L l1) `compare` (p1L l2, p2L l2)

x1L :: RealFrac a => Line a -> a
x1L = xP . p1L

y1L :: RealFrac a => Line a -> a
y1L = yP . p1L

x2L :: RealFrac a => Line a -> a
x2L = xP . p2L

y2L :: RealFrac a => Line a -> a
y2L = yP . p2L

-- The rest of this file is TOG-derived

invSlope :: RealFrac a => Line a -> a
invSlope l = (x2L l - x1L l) / (y2L l - y1L l)

xIntercept :: RealFrac a => Line a -> a -> a
xIntercept l is = x1L l - is * y1L l
                     
-- XXX Does not protect itself against nearly-parallel lines
intersect :: RealFrac a => Line a -> Line a -> Maybe (Point a)
intersect l1 l2 = 
  -- x = m1y + b1
  -- x = m2y + b2
  -- m1y + b1 = m2y + b2
  -- y * (m1 - m2) = b2 - b1
  -- y = (b2 - b1) / (m1 - m2)
  let m1 = invSlope l1
      m2 = invSlope l2 in
  if m1 - m2 == 0
  then Nothing
  else
      let b1 = xIntercept l1 m1
          b2 = xIntercept l2 m2
          y = (b2 - b1) / (m1 - m2) in
      Just $ Point {
        xP = m1 * y + b1,
        yP = y }

-- XXX Does not protect itself against horizontal lines
computeX :: RealFrac a => Line a -> a -> a
computeX line y =
  let dx = x2L line - x1L line in
  let ex = (y - y1L line) * dx in
  let dy = y2L line - y1L line in
  x1L line + ex / dy
      
data RealFrac a => Trap a = Trap {
  y1 :: a,
  y2 :: a,
  x11 :: a,
  x12 :: a,
  x21 :: a,
  x22 :: a }

polyEdges :: RealFrac a => [Point a] -> [Line a]
polyEdges =
  sort . map orderEdge . filter nonHorizontal . makeEdges . close
  where
    close [] = error "empty poly"
    close ps@(p : _) = ps ++ [p]
    makeEdges [] = error "internal error: empty poly"
    makeEdges [_] = []
    makeEdges (p1 : p2 : ps) =
      Line p1 p2 : makeEdges (p2 : ps)
    nonHorizontal edge = 
      y1L edge /= y2L edge
    orderEdge edge
      | p1L edge < p2L edge = edge
      | otherwise = Line (p2L edge) (p1L edge)

{-
-- XXX Even-odd fill rule only for now
    [] -> []
    [_] -> error "unpaired edge"
    es@(e1 : _) ->
      takeWhile ((y1L e1 ==) . y1L) es
-}

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
