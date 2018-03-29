#!/bin/bash

export HDF5_VER=1.10
export HDF5_VER_MINOR=1
export HDF5=${HDF5_VER}.${HDF5_VER_MINOR}

export HDF5_DIR=hdf5-${HDF5}

curl -L https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-${HDF5_VER}/${HDF5_DIR}/src/${HDF5_DIR}.tar.gz | tar xz

cd ${HDF5_DIR}

./configure --prefix=/usr/local/hdf5
make
make install

cd ..





