# Copyright © 2010 Bart Massey

.SUFFIXES:

hxlogo: nonexistent-hxlogo

nonexistent-hxlogo:
	ghc -Wall --make hxlogo.hs

test-polyutils: nonexistent-test-polyutils

nonexistent-test-polyutils:
	ghc -Wall --make test-polyutils.hs
