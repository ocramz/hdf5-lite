SRCDIR = ${CURDIR}/src/Data/HDF5
HDF5_INCLUDE = /usr/local/hdf5/include/

all:
	make c2hs
	stack build

c2hs:
	c2hs --cppopts='-I${HDF5_INCLUDE}' src/Data/HDF5/Internal/Types.chs



####




step1:
	stack ghc -- -optc -g ${SRCDIR}/Lite/Internal.hs -isrc/ -I${HDF5_INCLUDE}
	stack ghc -- -optc -g ${SRCDIR}/Lite.hs -isrc/ -I${HDF5_INCLUDE}









# DEADCODESTRIP := -fdata-sections -ffunction-sections -Wl,--gc-sections 

# step2:
# 	gcc -c -g -w $(DEADCODESTRIP) 
