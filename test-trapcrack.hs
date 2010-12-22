{-# LANGUAGE FlexibleInstances #-}
-- Copyright © 2010 Bart Massey

import Text.Printf

import RenderUtils

pointify :: Real a => (a, a) -> Point a
pointify = uncurry Point

myPoly :: [Point Double]
myPoly = map pointify $ [
  (500, 0),
  (1000, 400),
  (450, 1000),
  (0, 600) ]

-- instance Show (Line Double) where
--   show t = printf "<(%g,%g)-(%g,%g)>" (x1L t) (y1L t) (x2L t) (y2L t)
-- 
-- instance Show (Trap Double) where
--   show t = printf "<(%g,%g)-(%g,%g)/(%g,%g)-(%g,%g)>"
--                   (x11T t) (y1T t) (x12T t) (y1T t)
--                   (x21T t) (y2T t) (x22T t) (y2T t)


printT :: Trap Double -> String
printT t = 
  printf ("<polygon points=\"%g,%g %g,%g %g,%g %g,%g\"\n" ++
          " style=\"fill:#cccc;stroke:#000000;stroke-width:1\"/>\n")
         (x11T t) (y1T t) (x12T t) (y1T t)
         (x22T t) (y2T t) (x21T t) (y2T t)

-- http://www.w3schools.com/svg/tryit.asp?filename=polygon1&type=svg

prelude :: String
prelude =
  "<?xml version=\"1.0\" standalone=\"no\"?>\n" ++
  "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n" ++
  " \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n\n" ++
  "<svg width=\"100%\" height=\"100%\" version=\"1.1\"\n" ++
  " xmlns=\"http://www.w3.org/2000/svg\">\n\n"

postlude :: String
postlude = "</svg>\n"

main :: IO ()
main = do
  putStr prelude
  let es = polyEdges myPoly
  let ts = polyEdgeTraps es
  mapM_ (putStrLn . printT) ts
  putStr postlude
