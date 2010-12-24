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

-- | This module contains some basic 2D geometric primitives,
-- and some primitive computations on these primitives. In
-- particular, it has some quite limited functionality for
-- "cracking" a polygon into a list of trapezoids. A polygon
-- is simply represented as a vertex-list, with no special type.
-- 
-- While the module was written with the X Render Extension
-- in mind, there is deliberately nothing X-specific in here.
-- The basic coordinate type is left as loose as reasonably
-- possible to accommodate clients of all sorts.
module PolyUtils (Point(..), Line(..), Trap(..),
                  x1L, y1L, x2L, y2L,
                  closePoly, polyEdges, polyTraps,
                  invSlope, intersect)
where
  
import Data.List (sort)  

-- | This module's objects are all ordered, so we need
-- at least 'Real' as a constraint. The wacky access
-- function names are to avoid constantly colliding
-- with the names normally used in geometric computations.
data Real a => Point a = 
  Point { xP :: a, yP :: a } deriving Eq

-- | This ordering is completely counterintuitive.
-- It is really only intended for the polygon cracker.
-- This is a bug and should be fixed.
instance Real a => Ord (Point a) where
  p1 `compare` p2 = 
    cOrd p1 `compare` cOrd p2
    where
      cOrd p = (yP p, xP p)

data Real a => Line a = 
  Line { p1L :: Point a, p2L :: Point a } deriving Eq

-- | This ordering is completely counterintuitive.
-- It is really only intended for the polygon cracker.
-- This is a bug and should be fixed.
instance Real a => Ord (Line a) where
  l1 `compare` l2 = 
    pOrd l1 `compare` pOrd l2
    where
      pOrd l = (y1L l, x1L l, x2L l)

-- | Shorthand for 'xP' . 'p1L' . The others below are similar.
x1L :: Real a => Line a -> a
x1L = xP . p1L

y1L :: Real a => Line a -> a
y1L = yP . p1L

x2L :: Real a => Line a -> a
x2L = xP . p2L

y2L :: Real a => Line a -> a
y2L = yP . p2L

-- | 6-coordinate trapezoid representation.
data Real a => Trap a = Trap {
  y1T :: a, -- ^ Upper y
  y2T :: a, -- ^ Lower y
  x11T :: a, -- ^ Upper-left x
  x12T :: a, -- ^ Upper-right x
  x21T :: a, -- ^ Lower-left x
  x22T :: a -- ^ Lower-right x
}

-- | Close the given polygon off by repeating the first
-- point at the end.
closePoly :: [Point a] -> [Point a]
closePoly [] = error "empty poly"
closePoly ps@(p : _) = ps ++ [p]

-- | Trace around the polygon, which is assumed to be closed.
-- Return the edge list.
polyEdges :: Real a => [Point a] -> [Line a]
polyEdges =
  sort . map orderEdge . filter nonHorizontal . makeEdges
  where
    makeEdges [] = error "internal error: empty poly"
    makeEdges [_] = []
    makeEdges (p1 : p2 : ps) =
      Line p1 p2 : makeEdges (p2 : ps)
    nonHorizontal edge = 
      y1L edge /= y2L edge
    orderEdge edge
      | p1L edge < p2L edge = edge
      | otherwise = Line (p2L edge) (p1L edge)

-- | This function takes an edge list representing a
-- polygon, and returns a collection of trapezoids defining
-- the same filled polygon. The input edge list should be
-- closed, and should contain no horizontal edges.
-- 
-- How it works: Given all the non-horizontal edges of a
-- non-self-intersecting polygon, walk the edges off in
-- increasing starting y / increasing starting x
-- order. Because the polygon is non-self-intersecting,
-- you'll get pairs of non-horizontal edges that start at a
-- given y with least x: these define the top of your
-- trapezoid. Take the first pair, clip off the rest of the
-- trapezoid at the smaller ending y, and place the unused
-- portion of the lower edge back on the edge list in its
-- proper position. Repeat until no more edges are
-- available.
-- 
-- This traversal corresponds to the "odd-even fill
-- rule". It would only be a little more work to do
-- "nonzero-winding", but I don't need it and don't want to
-- think about it right now.  Besides, I'd need to think
-- about self-intersecting polygons and this code anyhow: it
-- would need at minimum to deal with "crossed" traps.
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

-- | This function takes a vertex list representing a
-- polygon, and returns a collection of trapezoids defining
-- the same filled polygon. The input vertex list should be
-- closed.
polyTraps :: RealFrac a => [Point a] -> [Trap a]
polyTraps = polyEdgeTraps . polyEdges

-- | Put the line into the list in its proper place.
insertLine :: Real a => Line a -> [Line a] -> [Line a]
insertLine l [] = [l]
insertLine l el@(e : es)
  | e < l = e : insertLine l es
  | otherwise = l : el
  
-- The rest of this file is TOG-derived

-- | Return the inverse of the slope of the given line.
-- The inverse slope is chosen on the assumption that the
-- line is not horizontal or near-horizontal, so don't.
invSlope :: RealFrac a => Line a -> a
invSlope l = (x2L l - x1L l) / (y2L l - y1L l)

xIntercept :: Real a => Line a -> a -> a
xIntercept l m = x1L l - m * y1L l
                     
-- | Given two line segments, calculate the intersection
-- point of the lines defined by the segments.  This does
-- not protect itself against nearly-parallel lines, but
-- does protect itself against exactly-parallel lines
-- by returning 'Nothing'.
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
