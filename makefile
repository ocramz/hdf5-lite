SRCDIR = ${CURDIR}/src/Data/HDF5
HDF5_INCLUDE = /usr/local/hdf5/include/

all:
	make c2hs
	stack build

c2hs:
	c2hs --cppopts='-I${HDF5_INCLUDE}' src/Data/HDF5/Internal/Types.chs



####




step1:
	stack ghc -- -optc -g ${SRCDIR}/Lite/Internal.hs ${SRCDIR}/Lite.hs -isrc/ -I${HDF5_INCLUDE}

step2:
	stack ghc -- src/Data/HDF5/Lite.o src/Data/HDF5/Lite/Internal.o -lhdf5 src/Data/HDF5/Lite.hs






