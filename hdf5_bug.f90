program hdf5_bug

  use hdf5
  implicit none

  character(len=*), parameter :: fname='output.h5'
  !integer, parameter :: dset_size=4000000, num_dsets=1
  integer, parameter :: DP=kind(1d0)
  character(len=16) :: arg
  integer :: dset_size, num_dsets
  integer :: errcode

  call get_command_argument(1, arg)
  read(arg,*) dset_size
  call get_command_argument(2, arg)
  read(arg,*) num_dsets

  print *, 'dset_size = ', dset_size
  print *, 'num_dsets = ', num_dsets

  call h5open_f(errcode)
  call check_hdf5_error(errcode)

  call prepare_file()
  call write_file()

  call h5close_f(errcode)
  call check_hdf5_error(errcode)

contains

subroutine prepare_file()
  integer(HID_T) :: file_id
  integer :: ii, errcode
  character(len=9) dset_name

  print *, 'Creating file ', fname
  call h5fcreate_f(fname, H5F_ACC_TRUNC_F, file_id, errcode)
  call check_hdf5_error(errcode)

  do ii = 1, num_dsets
    write(dset_name, ('(a,i0.4)')) 'dset_', ii
    print *, 'Creating dataset ', dset_name
    call hdf5_create_dset(file_id, dset_name, H5T_NATIVE_INTEGER, (/dset_size/))
  enddo

  call h5fclose_f(file_id, errcode)
  call check_hdf5_error(errcode)

end subroutine prepare_file


! Write stuff to file. This will cause the file to become corrupt.
subroutine write_file()
  integer(HID_T) :: file_id
  integer :: errcode
  character(len=9) dset_name
  integer :: ii, buf(dset_size)

  print *, 'Opening file ', fname
  call h5fopen_f(fname, H5F_ACC_RDWR_F, file_id, errcode)
  call check_hdf5_error(errcode)

  buf(:) = 0
  do ii = 1, num_dsets
    write(dset_name, ('(a,i0.4)')) 'dset_', ii
    print *, 'Writing dataset ', dset_name
    call hdf5_write_int_hyperslab(file_id, dset_name, buf)
  enddo

end subroutine write_file


!=============================================================================-
! Auxiliary routines
!==============================================================================

subroutine hdf5_write_int_hyperslab(loc_id, dset_name, buf)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(LEN=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(dset_size) :: buf

  integer :: errcode
  integer(HSIZE_T) :: hcountf(1) !< Count for file dataspace
  integer(HSIZE_T) :: hcountm(1) !< Count for memory dataspace
  integer(HSIZE_T) :: hoffsetf(1) !< Offset for file dataspace
  integer(HID_T) :: dset_id
  integer(HID_T) :: dataspace
  integer(HID_T) :: memspace

  call h5dopen_f(loc_id, dset_name, dset_id, errcode)
  call check_hdf5_error(errcode)

  ! FHJ: Get 2D file dataspace and set selection mask
  call h5dget_space_f(dset_id, dataspace, errcode)
  call check_hdf5_error(errcode)
  hcountf(:) = dset_size
  hoffsetf(:) = 0
  call h5sselect_hyperslab_f(dataspace, H5S_SELECT_SET_F, hoffsetf, hcountf, errcode)
  call check_hdf5_error(errcode)
  ! FHJ: Create flat memory dataspace
  hcountm(1) = dset_size
  call h5screate_simple_f(1, hcountm, memspace, errcode)
  call check_hdf5_error(errcode)

  call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, buf, hcountm, errcode, memspace, dataspace)
  call check_hdf5_error(errcode)

  call h5sclose_f(memspace, errcode)
  call check_hdf5_error(errcode)
  call h5sclose_f(dataspace, errcode)
  call check_hdf5_error(errcode)
  call h5dclose_f(dset_id, errcode)
  call check_hdf5_error(errcode)

end subroutine hdf5_write_int_hyperslab


! Creates an empty dataset
subroutine hdf5_create_dset(loc_id, dset_name, dtype, dims)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(LEN=*), intent(in) :: dset_name !< HDF5 dataset name
  integer(HID_T), intent(in) :: dtype
  integer, intent(in) :: dims(:)

  integer(HSIZE_T) :: hdims(size(dims))
  integer(HID_T) :: dset_id
  integer(HID_T) :: dspace
  integer(HID_T) :: plist_id
  integer :: errcode

  hdims(:) = dims(:)
  call h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, errcode)
  call check_hdf5_error(errcode)
  ! FHJ: The following file causes corrupted files on the Burst Buffer
  call h5pset_alloc_time_f(plist_id, H5D_ALLOC_TIME_EARLY_F, errcode)
  !call h5pset_layout_f(plist_id, H5D_CONTIGUOUS_F, errcode)
  call check_hdf5_error(errcode)
  call h5screate_simple_f(size(dims), hdims, dspace, errcode)
  call check_hdf5_error(errcode)
  call h5dcreate_f(loc_id, dset_name, dtype, dspace, dset_id, errcode, dcpl_id=plist_id)
  call check_hdf5_error(errcode)
  call h5pclose_f(plist_id, errcode)
  call check_hdf5_error(errcode)
  call h5dclose_f(dset_id, errcode)
  call check_hdf5_error(errcode)
  call h5sclose_f(dspace, errcode)
  call check_hdf5_error(errcode)

end subroutine hdf5_create_dset


! Make sure error code is non-zero
subroutine check_hdf5_error(errcode)
  integer, intent(in) :: errcode

  if (errcode/=0) then
    write(6,*) 'HDF5 error:', errcode
    stop 1
  endif

end subroutine check_hdf5_error

end program hdf5_bug
