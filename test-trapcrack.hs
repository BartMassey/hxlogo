{-# LANGUAGE FlexibleInstances #-}
-- Copyright © 2010 Bart Massey

import Text.Printf

import RenderUtils

pointify :: Real a => (a, a) -> Point a
pointify = uncurry Point

myPoly :: [Point Double]
myPoly = map pointify $ [
  (1, 0),
  (2, 0.8),
  (0.9, 2),
  (0, 1.2) ]

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
         (x21T t) (y2T t) (x22T t) (y2T t)

prelude :: [String]
prelude = [
  "<?xml version=\"1.0\" standalone=\"no\"?>",
  "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"",
  " \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">",
  "",
  "<svg width=\"100%\" height=\"100%\" version=\"1.1\"",
  " xmlns=\"http://www.w3.org/2000/svg\">", 
  "" ]

main :: IO ()
main = do
  putStr $ unlines prelude
  let es = polyEdges myPoly
  let ts = polyEdgeTraps es
  mapM_ (putStrLn . printT) ts
