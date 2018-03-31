{-# language OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
{-# language TypeFamilies #-}
module Data.HDF5.Lite.Internal.InlineC (
  -- * File
    fcreate
  , fopen
  , fclose
  , FileOpenMode(..)
  -- * Dataspace
  , screateSimple
  , sclose
  -- * Dataset
  -- ** Create and fill
  , makeDatasetDouble'
  , makeDatasetFloat'
  , makeDatasetInt'
  -- ** Read contents
  , readDatasetDouble'
  , readDatasetFloat'
  -- ** Get information
  , getDatasetInfo
  , dclose
  -- * Helpers
  , withPtr2
  , withPtr3
  , peekArrayPtr
  , peekVS

                                       ) where

import Control.Monad.Catch (throwM, MonadThrow(..))
import Control.Exception (Exception(..), bracket)

import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt, CChar, CDouble, CFloat, CSize)
-- import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray, peekArray, lengthArray0)
import Foreign.ForeignPtr (ForeignPtr(..), mallocForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable

import GHC.Prim

import qualified Language.C.Inline as C

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS (Vector(..), unsafeWith, unsafeFromForeignPtr0, unsafeToForeignPtr0, fromList)
import qualified Data.Vector.Storable.Mutable as VM

import Data.HDF5.Internal.Types (Herr, Hid, Hsize, H5T_class_t)
import Data.HDF5.Lite.Internal.InlineC.Context (hdf5ctx)
import Data.HDF5.Internal.Exceptions
import Data.HDF5.Store

C.context hdf5ctx

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
data FileOpenMode = ReadOnly | ReadWrite deriving (Eq, Show)

-- | Open a file
fopen :: String -> FileOpenMode -> IO Hid
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



-- * Dataspace

-- hid_t H5Screate_simple( int rank, const hsize_t * current_dims, const hsize_t * maximum_dims )
-- Purpose:
--     Creates a new simple dataspace and opens it for access.
-- Description:
--     H5Screate_simple creates a new simple dataspace and opens it for access, returning a dataspace identifier.
--     rank is the number of dimensions used in the dataspace.
--     current_dims is a one-dimensional array of size rank specifying the size of each dimension of the dataset. maximum_dims is an array of the same size specifying the upper limit on the size of each dimension.
--     Any element of current_dims can be 0 (zero). Note that no data can be written to a dataset if the size of any dimension of its current dataspace is 0. This is sometimes a useful initial state for a dataset.
--     maximum_dims may be the null pointer, in which case the upper limit is the same as current_dims. Otherwise, no element of maximum_dims should be smaller than the corresponding element of current_dims.
--     If an element of maximum_dims is H5S_UNLIMITED, the maximum size of the corresponding dimension is unlimited.
--     Any dataset with an unlimited dimension must also be chunked; see H5Pset_chunk. Similarly, a dataset must be chunked if current_dims does not equal maximum_dims.
--     The dataspace identifier returned from this function must be released with H5Sclose or resource leaks will occur.
-- Parameters:
--     int rank 	IN: Number of dimensions of dataspace.
--     const hsize_t * current_dims 	IN: Array specifying the size of each dimension.
--     const hsize_t * maximum_dims     	IN: Array specifying the maximum size of each dimension.
screateSimple :: CInt -> [Hsize] -> IO Hid
screateSimple rank dims =
  withArray dims $ \dims_ -> 
  [C.exp| hid_t{
      H5Screate_simple( $(int rank), $(hsize_t* dims_), NULL)
               }|]

sclose :: Hid -> IO Herr
sclose hid = [C.exp| herr_t{ H5Sclose( $(hid_t hid)) }|]


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
  [C.exp| herr_t{H5LTmake_dataset_double( $(hid_t loc), $(const char* name), $(int rank), $(const hsize_t* dims), $(double* bp) ) } |]  

makeDatasetFloat' :: Hid -> Ptr CChar -> CInt -> Ptr Hsize -> Ptr CFloat -> IO Herr
makeDatasetFloat' loc name rank dims bp =
  [C.exp| herr_t{ H5LTmake_dataset_float( $(hid_t loc), $(const char* name), $(int rank), $(const hsize_t* dims), $(float* bp) ) } |]

makeDatasetInt'
  :: Hid -> Ptr CChar -> CInt -> Ptr Hsize -> Ptr CInt -> IO Herr
makeDatasetInt' loc name rank dims bp =
  [C.exp| herr_t{
        H5LTmake_dataset_int( $(hid_t loc), $(const char* name), $(int rank), $(const hsize_t* dims), $(int* bp) ) } |]


-- | Close a dataset and release its resources
dclose :: Hid -> IO Herr
dclose did = [C.exp| herr_t{ H5Dclose( $(hid_t did)) } |]


-- herr_t H5LTread_dataset_double ( hid_t loc_id, const char *dset_name, double *buffer  
-- Purpose:
--     Reads a dataset from disk. 
-- Description:
--     H5LTread_dataset reads a dataset named dset_name attached to the object specified by the identifier loc_id. The HDF5 datatype is H5T_NATIVE_DOUBLE. 
-- Parameters:
-- hid_t loc_id
--     IN: Identifier of the file or group to read the dataset within. 
-- const char *dset_name
--     IN: The name of the dataset to read. 
-- double * buffer
--     OUT: Buffer with data.


readDatasetDouble' loc name_ bp =
  [C.exp|herr_t{
      H5LTread_dataset_double( $(hid_t loc), $(const char* name_), $(double* bp) )
               }|]

readDatasetFloat' loc name_ bp =
  [C.exp|herr_t{
      H5LTread_dataset_float( $(hid_t loc), $(const char* name_), $(float* bp) )
               }|]


-- herr_t H5LTget_attribute_ndims( hid_t loc_id, const char *obj_name, const char *attr_name,  int *rank )
-- Purpose:
--     Gets the dimensionality of an attribute. 
-- Description:
--     H5LTget_attribute_ndims gets the dimensionality of an attribute named attr_name that is attached to the object specified by the name obj_name. 
-- Parameters:
-- hid_t loc_id
--     IN: Identifier of the object ( dataset or group) to read the attribute from. 
-- const char *obj_name
--     IN: The name of the object that the attribute is attached to. 
-- const char *attr_name
--     IN: The attribute name. 
-- int * rank
--     OUT: The dimensionality of the attribute. 







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
getDatasetInfo :: Hid -> String -> IO ([Hsize], H5T_class_t, CSize)
getDatasetInfo loc name = do 
  (sizes, (clid, szt)) <- peekArrayPtr (undefined :: Hsize) $ \dims -> 
      withPtr2 $ \ clid szt ->
        withCString name $ \name_ ->                    
        [C.exp| herr_t{
        H5LTget_dataset_info ( $(hid_t loc), $(const char *name_), $(hsize_t *dims), $(H5T_class_t *clid), $(size_t *szt) )
                  }|]
  return (sizes, clid, szt)   




     

-- * Helpers




class Sized v where
  type SizedData v :: * 
  dat :: v -> SizedData v
  size :: v -> [Hsize]
  rank :: v -> CInt

-- data Sized v = Sized { szDat :: v, szDims :: [Hsize] } deriving (Eq, Show)



-- | Allocate two pointers, run an action using them and rethrow HDF5 errors as exceptions
withPtr2
  :: (Storable a, Storable b) =>
     (Ptr a -> Ptr b -> IO Herr) -> IO (a, b)
withPtr2 io = do
  (a, (b, _)) <- C.withPtr $ \a -> C.withPtr $ \b -> throwH (io a b)
  return (a, b)

-- | Allocate three pointers, run an action using them and rethrow HDF5 errors as exceptions
withPtr3
  :: (Storable a, Storable b, Storable c) =>
     (Ptr a -> Ptr b -> Ptr c -> IO Herr) -> IO (a, b, c)
withPtr3 io = do
  (a, (b, (c, _))) <- C.withPtr $ \a -> C.withPtr $ \b -> C.withPtr $ \c -> throwH (io a b c)
  return (a, b, c)



peekArrayPtr :: (Storable a, Eq a) => a -> (Ptr a -> IO b) -> IO ([a], b)
peekArrayPtr x io =
  snd <$> C.withPtr ( \p -> do
    e <- io p                         
    n <- lengthArray0 x p
    z <- peekArray n p
    return (z, e)
            )

-- | O(N) Copy memory into a Storable array (via fromList)
peekVS :: (Storable a, Eq a) =>
     a -> (Ptr a -> IO b) -> IO (VS.Vector a, b)
peekVS x io = do
  (arr, e) <- peekArrayPtr x io
  return (VS.fromList arr, e)
    





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
