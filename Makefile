# Copyright © 2010 Bart Massey

.SUFFIXES:

hxlogo: nonexistent

nonexistent:
	ghc -Wall --make hxlogo.hs
