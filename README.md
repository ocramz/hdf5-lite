# hdf5-lite

[![Build Status](https://travis-ci.org/ocramz/hdf5-lite.png)](https://travis-ci.org/ocramz/hdf5-lite)

Bindings to the HDF5 "lite" interface: https://support.hdfgroup.org/HDF5/doc/HL/RM_H5LT.html

## Dependencies

- The HDF5 library headers must be correctly installed at `/usr/local/hdf5/include`. In particular, the file `/usr/local/hdf5/include/hdf5_hl.h` must be present

- The `stack` build tool

- The `c2hs` tool (in case you don't have it, `stack install c2hs`)

## Installation

- `make`
  - generates the Haskell types by running `c2hs`. This is necessary since HDF5 may be configured to use specific numerical precisions etc.
  - runs `stack build`
