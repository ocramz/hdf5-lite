{-# language OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
{-# language TypeFamilies #-}
module Data.HDF5.Lite where

import Data.Functor ((<$>))

import Control.Monad.Catch (throwM, MonadThrow(..))
import Control.Exception (Exception(..), bracket)

import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt, CChar, CDouble, CFloat)
-- import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable

import GHC.Prim

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VM

import           Data.HDF5.Internal.Types (Herr, Hid, Hsize)
import qualified Data.HDF5.Lite.Internal as H
import qualified Language.C.Inline as C

import Data.HDF5.Internal.Exceptions

C.context H.hdf5ctx

C.include "<hdf5.h>"
C.include "<hdf5_hl.h>"


  

-- | HDF5 Lite API reference: https://support.hdfgroup.org/HDF5/doc/HL/RM_H5LT.html

-- hid_t H5Fcreate( const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id )
-- Purpose:
--     Creates an HDF5 file.
-- Description:
--     H5Fcreate is the primary function for creating HDF5 files; it creates a new HDF5 file with the specified name and property lists and specifies whether an existing file of same name should be overwritten.
--     The name parameter specifies the name of the new file.
--     The flags parameter specifies whether an existing file is to be overwritten. It should be set to either H5F_ACC_TRUNC to overwrite an existing file or H5F_ACC_EXCL, instructing the function to fail if the file already exists.
--     New files are always created in read-write mode, so the read-write and read-only flags, H5F_ACC_RDWR and H5F_ACC_RDONLY, respectively, are not relevant in this function. Further note that a specification of H5F_ACC_RDONLY will be ignored; the file will be created in read-write mode, regardless.
--     More complex behaviors of file creation and access are controlled through the file creation and file access property lists, fcpl_id and fapl_id, respectively. The value of H5P_DEFAULT for any property list value indicates that the library should use the default values for that appropriate property list.
--     The return value is a file identifier for the newly-created file; this file identifier should be closed by calling H5Fclose when it is no longer needed. 
fcreate :: String -> IO Hid
fcreate name = withCString name $ \name_ ->
  [C.exp| hid_t{ H5Fcreate( $(const char* name_), H5F_ACC_EXCL, H5P_DEFAULT, H5P_DEFAULT)}|]



-- * File operations (H5F)
-- NB: each HDF5 physical file (e.g. "/usr/data/dataset0.h5" ) specifies a whole internal filesystem, the elements of which are what we call "files" here

-- hid_t H5Fopen( const char *name, unsigned flags, hid_t fapl_id )
-- Purpose:
--     Opens an existing HDF5 file.
-- Description:
--     H5Fopen is the primary function for accessing existing HDF5 files. This function opens the named file in the specified access mode and with the specified access property list.
--     Note that H5Fopen does not create a file if it does not already exist; see H5Fcreate.
--     The name parameter specifies the name of the file to be opened.
--     The fapl_id parameter specifies the file access property list. Use of H5P_DEFAULT specifies that default I/O access properties are to be used
--     The flags parameter specifies whether the file will be opened in read-write or read-only mode, H5F_ACC_RDWR or H5F_ACC_RDONLY, respectively. More complex behaviors of file access are controlled through the file-access property list.
--     The return value is a file identifier for the open file; this file identifier should be closed by calling H5Fclose when it is no longer needed. 


-- hid_t H5Fopen( const char *name, unsigned flags, hid_t fapl_id )
fopenReadOnly :: String -> IO Hid
fopenReadOnly name = withCString name $ \name_ -> 
  [C.exp| hid_t{ H5Fopen( $(const char* name_), H5F_ACC_RDONLY, H5P_DEFAULT )}|]

fopenRW :: String -> IO Hid
fopenRW name = withCString name $ \name_ ->
  [C.exp| hid_t{ H5Fopen( $(const char* name_), H5F_ACC_RDWR, H5P_DEFAULT )}|]

-- | File access mode
data FMode = ReadOnly | ReadWrite deriving (Eq, Show)

-- | Open a file
fopen :: String -> FMode -> IO Hid
fopen name mode =
  case mode of ReadOnly -> fopenReadOnly name
               ReadWrite -> fopenRW name


-- herr_t H5Fclose( hid_t file_id )
-- Purpose:
--     Terminates access to an HDF5 file.
-- Description:
--     H5Fclose terminates access to an HDF5 file by flushing all data to storage and terminating access to the file through file_id.
--     If this is the last file identifier open for the file and no other access identifier is open (e.g., a dataset identifier, group identifier, or shared datatype identifier), the file will be fully closed and access will end.
--     Delayed close:
--     Note the following deviation from the above-described behavior. If H5Fclose is called for a file but one or more objects within the file remain open, those objects will remain accessible until they are individually closed. Thus, if the dataset data_sample is open when H5Fclose is called for the file containing it, data_sample will remain open and accessible (including writable) until it is explicitely closed. The file will be automatically closed once all objects in the file have been closed.
--     Be warned, however, that there are circumstances where it is not possible to delay closing a file. For example, an MPI-IO file close is a collective call; all of the processes that opened the file must close it collectively. The file cannot be closed at some time in the future by each process in an independent fashion. Another example is that an application using an AFS token-based file access privilage may destroy its AFS token after H5Fclose has returned successfully. This would make any future access to the file, or any object within it, illegal.
--     In such situations, applications must close all open objects in a file before calling H5Fclose. It is generally recommended to do so in all cases.
-- Parameters:
--     hid_t file_id     	IN: Identifier of a file to terminate access to.
fclose :: Hid -> IO Herr
fclose fid = [C.exp| herr_t{ H5Fclose( $(hid_t fid))}|]


-- | File memory bracket, preexisting file
withFile :: String -> FMode -> (Hid -> IO c) -> IO c
withFile name mode = bracket (fopen name mode) fclose


-- | File memory bracket
withFileCreate :: String -> (Hid -> IO c) -> IO c
withFileCreate name = bracket (fcreate name) fclose




-- * Dataspace (H5S)





-- * Dataset (H5D)

-- herr_t H5LTmake_dataset_double ( hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const double *buffer )
-- Purpose:
--     Creates and writes a dataset.  
-- Description:
--     H5LTmake_dataset creates and writes a dataset named dset_name attached to the object specified by the identifier loc_id.
--     The dataset’s datatype will be native floating-point double, H5T_NATIVE_DOUBLE. 
-- Parameters:
-- hid_t loc_id
--     IN: Identifier of the file or group to create the dataset within. 
-- const char *dset_name
--     IN: The name of the dataset to create. 
-- int rank
--     IN: Number of dimensions of dataspace. 
-- const hsize_t * dims
--     IN: An array of the size of each dimension. 
-- const double * buffer
--     IN: Buffer with data to be written to the dataset.
-- Returns:
--     Returns a non-negative value if successful; otherwise returns a negative value.
makeDatasetDouble' :: Hid -> Ptr CChar -> CInt -> Ptr Hsize -> Ptr C.CDouble -> IO Herr
makeDatasetDouble' loc name rank dims bp =
  [C.exp| herr_t{
        H5LTmake_dataset_double( $(hid_t loc), $(const char* name), $(int rank), $(const hsize_t* dims), $(double* bp) ) } |]  


makeDatasetDouble
  :: Hid -> String -> [Hsize] -> VS.Vector C.CDouble -> IO ()
makeDatasetDouble loc name dims buffer =
  withMakeDataset loc name dims buffer makeDatasetDouble'


-- float

makeDatasetFloat' :: Hid -> Ptr CChar -> CInt -> Ptr Hsize -> Ptr CFloat -> IO Herr
makeDatasetFloat' loc name rank dims bp =
  [C.exp| herr_t{
        H5LTmake_dataset_float( $(hid_t loc), $(const char* name), $(int rank), $(const hsize_t* dims), $(float* bp) ) } |]

makeDatasetFloat
  :: Hid -> String -> p -> [Hsize] -> VS.Vector CFloat -> IO ()
makeDatasetFloat loc name rank dims buffer =
  withMakeDataset loc name dims buffer makeDatasetFloat'

-- int

makeDatasetInt'
  :: Hid -> Ptr CChar -> CInt -> Ptr Hsize -> Ptr CInt -> IO Herr
makeDatasetInt' loc name rank dims bp =
  [C.exp| herr_t{
        H5LTmake_dataset_int( $(hid_t loc), $(const char* name), $(int rank), $(const hsize_t* dims), $(int* bp) ) } |]

makeDatasetInt
  :: Hid -> String -> [Hsize] -> VS.Vector CInt -> IO ()
makeDatasetInt loc name dims bp =
  withMakeDataset loc name dims bp makeDatasetInt'
  

-- | Helper function for dataset creation
withMakeDataset :: (Num t1, Storable a1, Storable a2) =>
     t2
     -> String
     -> [a1]
     -> VS.Vector a2
     -> (t2 -> CString -> t1 -> Ptr a1 -> Ptr a2 -> IO Herr)
     -> IO ()
withMakeDataset loc name dims buffer io =
    let
    rank = fromIntegral $ length dims
  in 
    withCString name $ \name_ ->
    withArray dims $ \dims_ -> 
    VS.unsafeWith buffer $ \ bp_ -> throwH (io loc name_ rank dims_ bp_)


-- herr_t H5LTmake_dataset_char ( hid_t loc_id, const char *dset_name, int rank, const hsize_t *dims, const char *buffer )


-- | Close a dataset and release its resources
dclose :: Hid -> IO Herr
dclose did = [C.exp| herr_t{ H5Dclose( $(hid_t did)) } |]







-- herr_t H5LTget_dataset_info ( hid_t loc_id, const char *dset_name, hsize_t *dims, H5T_class_t *class_id, size_t *type_size )
-- Purpose:
--     Gets information about a dataset.  
-- Description:
--     H5LTget_dataset_info gets information about a dataset named dset_name exists attached to the object loc_id.
-- Parameters:
-- hid_t loc_id
--     IN: Identifier of the object to locate the dataset within. 
-- const char *dset_name
--     IN: The dataset name. 
-- hsize_t * dims
--     OUT: The dimensions of the dataset.
-- H5T_class_t * class_id
--     OUT: The class identifier. To a list of the HDF5 class types please refer to the Datatype Interface API help.
-- size_t * type_size
--     OUT: The size of the datatype in bytes. 
getDatasetInfo loc name = withCString name $ \name_ ->
  C.withPtr $ \dims ->
  C.withPtr $ \clid ->
  C.withPtr $ \szt ->
    [C.exp| herr_t{
        H5LTget_dataset_info ( $(hid_t loc), $(const char *name_), $(hsize_t *dims), $(H5T_class_t *clid), $(size_t *szt) )
                  }|]





-- * HDF5 image operations
-- The HDF5 Image API defines a standard storage for HDF5 datasets that are intended to be interpreted as images. The specification for this API is presented in another document: HDF5 Image and Palette Specification. This version of the API is primarily concerned with two dimensional raster data similar to HDF4 Raster Images. The HDF5 image API uses the Lite HDF5 API. 

-- herr_t H5IMread_image ( hid_t loc_id, const char *dset_name, unsigned char *buffer ) 
-- Purpose:
--     Reads image data from disk. 
-- Description:
--     H5IMread_image reads a dataset named dset_name attached to the file or group specified by the identifier loc_id. 
-- Parameters:
--     hid_t loc_id 	IN: Identifier of the file or group to read the dataset from.
--     const char *dset_name 	IN: The name of the dataset.
--     unsigned char *buffer     	OUT: Buffer with data to store the image.
-- Returns:
--     Returns a non-negative value if successful; otherwise returns a negative value.

-- readImage locId name = undefined




-- | Helpers

class Sized v where
  type SizedData v :: * 
  dat :: v -> SizedData v
  size :: v -> [Hsize]
  rank :: v -> CInt

-- data Sized v = Sized { szDat :: v, szDims :: [Hsize] } deriving (Eq, Show)
