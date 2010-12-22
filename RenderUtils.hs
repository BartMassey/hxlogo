-- Copyright © 2010 Bart Massey
-- Some code derived from material copyright The Open Group.
-- See the end of this file for license information
-- and original copyright notice.

module RenderUtils (Point(..), Line(..), Trap(..),
                    x1L, y1L, x2L, y2L,
                    invSlope, xIntercept, intersect,
                    polyEdges, polyTraps)
where
  
import Data.List (sort)  

data Real a => Point a = 
  Point { xP :: a, yP :: a } deriving Eq

instance Real a => Ord (Point a) where
  p1 `compare` p2 = 
    cOrd p1 `compare` cOrd p2
    where
      cOrd p = (yP p, xP p)

data Real a => Line a = 
  Line { p1L :: Point a, p2L :: Point a } deriving Eq

instance Real a => Ord (Line a) where
  l1 `compare` l2 = 
    pOrd l1 `compare` pOrd l2
    where
      pOrd l = (y1L l, x1L l, x2L l)

x1L :: Real a => Line a -> a
x1L = xP . p1L

y1L :: Real a => Line a -> a
y1L = yP . p1L

x2L :: Real a => Line a -> a
x2L = xP . p2L

y2L :: Real a => Line a -> a
y2L = yP . p2L

data Real a => Trap a = Trap {
  y1T :: a,
  y2T :: a,
  x11T :: a,
  x12T :: a,
  x21T :: a,
  x22T :: a }

-- Trace around the polygon and close it off by connecting
-- the last point to the first.
polyEdges :: Real a => [Point a] -> [Line a]
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

-- Given the edge list of a simple polygon, walk the
-- edges off in increasing starting y / increasing starting x
-- order. Because the polygon is simple, you'll get pairs of
-- non-horizontal edges that start at a given y with least x:
-- these define the top of your trapezoid. Take the first pair,
-- clip off the rest of the trapezoid at the smaller ending y,
-- and place the unused portion of the lower edge back on the
-- edge list in its proper position. Repeat until no more edges
-- are available.
-- 
-- This traversal corresponds to the "odd-even fill rule". It
-- would only be a little more work to do "nonzero-winding", but
-- I don't need it and don't want to think about it right now.
-- Besides, I'd need to think about non-simple polygons and this
-- code anyhow: it would need at minimum to deal with "crossed"
-- traps.
polyEdgeTraps :: RealFrac a => [Line a] -> [Trap a]
polyEdgeTraps [] = []
polyEdgeTraps [_] = error "unpaired edge"
polyEdgeTraps (e1 : e2 : es)
  | y1L e1 /= y1L e2 = error "mispaired edge"
  | y2L e1 == y2L e2 =
    Trap {
      y1T = y1L e1,
      y2T = y2L e1,
      x11T = x1L e1,
      x12T = x1L e2,
      x21T = x2L e1,      
      x22T = x2L e2 } : 
    polyEdgeTraps es
  | y2L e1 < y2L e2 =
    let x2 = x1L e2
        y1 = y1L e1
        y2 = y2L e1 in
    let x2' = x2 + (y2 - y1) * (x2L e2 - x2) / (y2L e2 - y1) in
    Trap {
      y1T = y1,
      y2T = y2,
      x11T = x1L e1,
      x12T = x2,
      x21T = x2L e1,
      x22T = x2' } : 
    polyEdgeTraps (insertLine (Line (Point x2' y2) (p2L e2)) es)
  | otherwise =
    let x1 = x1L e1
        y1 = y1L e2
        y2 = y2L e2 in
    let x1' = x1 + (y2 - y1) * (x2L e1 - x1) / (y2L e1 - y1) in
    Trap {
      y1T = y1,
      y2T = y2,
      x11T = x1,
      x12T = x1L e2,
      x21T = x1',
      x22T = x2L e2 } : 
    polyEdgeTraps (insertLine (Line (Point x1' y2) (p2L e1)) es)

polyTraps :: RealFrac a => [Point a] -> [Trap a]
polyTraps = polyEdgeTraps . polyEdges

-- Put the line into the list in its proper place.
insertLine :: Real a => Line a -> [Line a] -> [Line a]
insertLine l [] = [l]
insertLine l el@(e : es)
  | e < l = e : insertLine l es
  | otherwise = l : el
  
-- The rest of this file is TOG-derived

invSlope :: RealFrac a => Line a -> a
invSlope l = (x2L l - x1L l) / (y2L l - y1L l)

xIntercept :: Real a => Line a -> a -> a
xIntercept l m = x1L l - m * y1L l
                     
-- XXX Does not protect itself against nearly-parallel lines.
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
