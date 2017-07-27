program io_bug

  use hdf5
  implicit none

  character(len=*), parameter :: fname='output.h5'
  integer, parameter :: nq=9, ng=60000
  !integer, parameter :: nq=15, ng=60000
  !integer, parameter :: nq=15, ng=150000
  integer, parameter :: DP=kind(1d0)
  integer :: errcode

  call h5open_f(errcode)
  call check_hdf5_error(errcode)

  call prepare_file()
  call write_file()

  call h5close_f(errcode)
  call check_hdf5_error(errcode)

contains

subroutine prepare_file()
  integer(HID_T) :: file_id
  integer :: error

  call h5fcreate_f(fname, H5F_ACC_TRUNC_F, file_id, errcode)
  call check_hdf5_error(errcode)

  call hdf5_create_group(file_id, 'eps_header')
  call hdf5_create_group(file_id, 'eps_header/gspace')

  call hdf5_create_dset(file_id, 'eps_header/gspace/dset1', H5T_NATIVE_INTEGER, (/ng,nq/))
  call hdf5_create_dset(file_id, 'eps_header/gspace/dset2', H5T_NATIVE_INTEGER, (/ng,nq/))
  call hdf5_create_dset(file_id, 'eps_header/gspace/dset3', H5T_NATIVE_INTEGER, (/ng,nq/))
  call hdf5_create_dset(file_id, 'eps_header/gspace/dset4', H5T_NATIVE_INTEGER, (/ng,nq/))

  call h5fclose_f(file_id, error)
  call check_hdf5_error(errcode)

end subroutine prepare_file


! Write stuff to file. This will cause the file to become corrupt.
subroutine write_file()
  integer(HID_T) :: file_id
  integer :: errcode, countf(2), offsetf(2)

  integer :: ind(ng)
  real(DP) :: ekin(ng)

  call h5fopen_f(fname, H5F_ACC_RDWR_F, file_id, errcode)
  call check_hdf5_error(errcode)

  countf(:) = (/ng, 1/)
  offsetf(:) = (/0, 0/)
  ind(:) = 0
  ekin(:) = 0d0

  call hdf5_write_int_hyperslab(file_id, 'eps_header/gspace/dset1', &
    countf, offsetf, ind)
  call hdf5_write_int_hyperslab(file_id, 'eps_header/gspace/dset2', &
    countf, offsetf, ind)
  call hdf5_write_int_hyperslab(file_id, 'eps_header/gspace/dset3', &
    countf, offsetf, ind)
  call hdf5_write_int_hyperslab(file_id, 'eps_header/gspace/dset4', &
    countf, offsetf, ind)

end subroutine write_file


!=============================================================================-
! Auxiliary routines
!==============================================================================

subroutine hdf5_write_int_hyperslab(loc_id, dset_name, countf, offsetf, buf)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(LEN=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention
  integer, intent(in) :: countf(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offsetf(:)
  !> Data buffer. We treat it as a flat contiguous 1D array.
  integer, intent(in), dimension(*) :: buf

  integer :: errcode
  integer(HSIZE_T) :: hcountf(size(countf)) !< Count for file dataspace
  integer(HSIZE_T) :: hcountm(1) !< Count for memory dataspace
  integer(HSIZE_T) :: hoffsetf(size(offsetf)) !< Offset for file dataspace
  integer(HID_T) :: dset_id
  integer(HID_T) :: dataspace
  integer(HID_T) :: memspace
  
  call h5dopen_f(loc_id, dset_name, dset_id, errcode)
  call check_hdf5_error(errcode)

  ! FHJ: Get 2D file dataspace and set selection mask
  call h5dget_space_f(dset_id, dataspace, errcode)
  call check_hdf5_error(errcode)
  hcountf(:) = countf(:)
  hoffsetf(:) = offsetf(:)
  call h5sselect_hyperslab_f(dataspace, H5S_SELECT_SET_F, hoffsetf, hcountf, errcode)
  call check_hdf5_error(errcode)
  ! FHJ: Create flat memory dataspace
  hcountm(1) = product(countf)
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
  !if (.false.) then
  if (.true.) then
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
  else
    call h5screate_simple_f(size(dims), hdims, dspace, errcode)
    call check_hdf5_error(errcode)
    call h5dcreate_f(loc_id, dset_name, dtype, dspace, dset_id, errcode)
    call check_hdf5_error(errcode)
  endif
  call h5dclose_f(dset_id, errcode)
  call check_hdf5_error(errcode)
  call h5sclose_f(dspace, errcode)
  call check_hdf5_error(errcode)

end subroutine hdf5_create_dset


!> Creates an empty group
subroutine hdf5_create_group(loc_id, group_name)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(LEN=*), intent(in) :: group_name !< HDF5 group name
  
  integer(HID_T) :: group_id
  integer :: errcode

  call h5gcreate_f(loc_id, group_name, group_id, errcode)
  call check_hdf5_error(errcode)
  call h5gclose_f(group_id, errcode)
  call check_hdf5_error(errcode)
  
end subroutine hdf5_create_group


! Make sure error code is non-zero
subroutine check_hdf5_error(errcode)
  integer, intent(in) :: errcode

  if (errcode/=0) then
    write(6,*) 'HDF5 error:', errcode
    stop
  endif

end subroutine check_hdf5_error

end program io_bug
