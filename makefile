SRCDIR = ${CURDIR}/src/Data/HDF5
SRCDIR_I = ${CURDIR}/src/Data/HDF5/Internal
SRCDIR_L = ${CURDIR}/src/Data/HDF5/Lite
HDF5_INCLUDE = /usr/local/hdf5/include/

all:
	make c2hs
	make step1
	stack build

c2hs:
	c2hs --cppopts='-I${HDF5_INCLUDE}' src/Data/HDF5/Internal/Types.chs


clean:
	rm ${SRCDIR}/*.o ${SRCDIR}/*.hi ${SRCDIR}/*.dyn* ${SRCDIR_I}/*.h ${SRCDIR_I}/*.o ${SRCDIR_I}/*.hi ${SRCDIR_I}/*.dyn* ${SRCDIR_L}/*.o ${SRCDIR_L}/*.hi ${SRCDIR_L}/*.dyn*


####




step1:
	stack ghc -- -optc -g ${SRCDIR}/Lite/Internal.hs ${SRCDIR}/Lite.hs -isrc/ -I${HDF5_INCLUDE}

step2:
	stack ghc -- src/Data/HDF5/Lite.o src/Data/HDF5/Lite/Internal.o -lhdf5 src/Data/HDF5/Lite.hs






