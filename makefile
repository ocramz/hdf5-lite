SRCDIR = ${CURDIR}/src/Data/HDF5

all:
	make c2hs
	stack build

c2hs:
	c2hs --cppopts='-I/usr/local/hdf5/include/' src/Data/HDF5/Internal/Types.chs


step1:
	stack ghc -- -optc -g ${SRCDIR}/Lite/Internal.hs -isrc/ -I/usr/local/hdf5/include

DEADCODESTRIP := -fdata-sections -ffunction-sections -Wl,--gc-sections

# step2:
# 	gcc -c -g -w $(DEADCODESTRIP) 
