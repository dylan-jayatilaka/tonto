module h5output

  use h5lt
  use hdf5
  use iso_c_binding

  implicit none

  public

  interface create_dataset_vec
    module procedure cd_real_4_1d
    module procedure cd_real_8_1d
    module procedure cd_int_4_1d
    module procedure cd_int_8_1d
    module procedure cd_complex_4_1d
    module procedure cd_complex_8_1d
  end interface

  interface create_dataset_mat
    module procedure cd_real_4_2d
    module procedure cd_real_8_2d
    module procedure cd_int_4_2d
    module procedure cd_int_8_2d
  end interface


contains

  ! create file procedure
  subroutine init_hdf
    integer :: hdferr

    call H5open_f(hdferr)
    
  end subroutine


  subroutine close_hdf
    integer :: hdferr
    call H5close_f(hdferr)
  end subroutine


  ! creates a hdf file that truncates any file with the same name
  subroutine create_hdf_file(filename, loc_id)
    character(len=*), intent(in) :: filename
    integer, intent(out) :: loc_id
    integer(hid_t) :: hdf_id
    integer :: hdferr

    call h5fcreate_f(filename, H5F_ACC_TRUNC_F, hdf_id, hdferr)

    loc_id = hdf_id

  end subroutine

  ! Unfortunately, due to lack of real generics in fortran we must
  ! write a function to instantiate the interface for every data
  ! type we have to

  subroutine cd_real_4_1d(loc_id, dataset_name, buffer)
    integer, intent(in) :: loc_id
    character(len=*), intent(in) :: dataset_name
    real(4), dimension(:), intent(in) :: buffer
    integer :: hdferr
    integer(hid_t) :: hdf_id
    integer(hsize_t), dimension(1) :: dims

    dims = shape(buffer)
    hdf_id = loc_id

    call h5ltmake_dataset_float_f(hdf_id, dataset_name, 1, dims, buffer, hdferr)

  end subroutine
  

  subroutine cd_real_8_1d(loc_id, dataset_name, buffer)
    integer, intent(in) :: loc_id
    character(len=*), intent(in) :: dataset_name
    real(8), dimension(:), intent(in) :: buffer
    integer :: hdferr
    integer(hid_t) :: hdf_id
    integer(hsize_t), dimension(1) :: dims

    dims = shape(buffer)
    hdf_id = loc_id

    call h5ltmake_dataset_double_f(hdf_id, dataset_name, 1, dims, buffer, hdferr)

  end subroutine

  
  subroutine cd_complex_4_1d(loc_id, dataset_name, buffer)
    integer, intent(in) :: loc_id
    character(len=*), intent(in) :: dataset_name
    complex(4), dimension(:), target, intent(in) :: buffer
    integer :: hdferr
    integer(hid_t) :: hdf_id, type_id, dspace_id, kind_type, dset_id
    integer(hsize_t), dimension(1) :: dims
    integer(hsize_t) :: offset
    type(c_ptr) :: f_ptr

    dims = shape(buffer)
    hdf_id = loc_id

    offset = c_sizeof(buffer(1))

    ! Create a dataspace (aka r/w buffer)
    call h5screate_simple_f(1, dims, dspace_id, hdferr)
    
    
    ! create the memory datatype
    call h5tcreate_f(H5T_COMPOUND_F, offset , type_id, hdferr)
    offset = 0
    call h5tinsert_f(type_id, "re", offset, H5T_NATIVE_REAL, hdferr)
    offset = c_sizeof(buffer(1)) / 2
    call h5tinsert_f(type_id, "im", offset, H5T_NATIVE_REAL, hdferr)
    

    ! create a dataset in the file to write to
    call h5dcreate_f(hdf_id, dataset_name, type_id, dspace_id, dset_id, hdferr)

    ! this is dangerous(assumes array indexed from 1,1
    f_ptr = C_LOC(buffer(1))

    ! write the buffer to file
    CALL h5dwrite_f(dset_id, type_id, f_ptr, hdferr)

    ! close both the dataset and the dataspace
    CALL h5dclose_f(dset_id, hdferr)
    CALL h5sclose_f(dspace_id, hdferr)

  end subroutine

  subroutine cd_complex_8_1d(loc_id, dataset_name, buffer)
    integer, intent(in) :: loc_id
    character(len=*), intent(in) :: dataset_name
    complex(8), dimension(:), target, intent(in) :: buffer
    integer :: hdferr
    integer(hid_t) :: hdf_id, type_id, dspace_id, kind_type, dset_id
    integer(hsize_t), dimension(1) :: dims
    integer(hsize_t) :: offset
    double precision :: b = 1.0
    type(c_ptr) :: f_ptr

    dims = shape(buffer)
    hdf_id = loc_id

    offset = c_sizeof(buffer(1))

    ! Create a dataspace (aka r/w buffer)
    call h5screate_simple_f(1, dims, dspace_id, hdferr)
    
    
    ! create the memory datatype
    call h5tcreate_f(H5T_COMPOUND_F, offset , type_id, hdferr)
    offset = 0
    call h5tinsert_f(type_id, "re", offset, H5T_NATIVE_DOUBLE, hdferr)
    offset = c_sizeof(buffer(1)) / 2
    call h5tinsert_f(type_id, "im", offset, H5T_NATIVE_DOUBLE, hdferr)
    

    ! create a dataset in the file to write to
    call h5dcreate_f(hdf_id, dataset_name, type_id, dspace_id, dset_id, hdferr)

    ! this is dangerous(assumes array indexed from 1,1
    f_ptr = C_LOC(buffer(1))

    ! write the buffer to file
    CALL h5dwrite_f(dset_id, type_id, f_ptr, hdferr)

    ! close both the dataset and the dataspace
    CALL h5dclose_f(dset_id, hdferr)
    CALL h5sclose_f(dspace_id, hdferr)

  end subroutine


  subroutine cd_int_4_1d(loc_id, dataset_name, buffer)
    integer, intent(in) :: loc_id
    character(len=*), intent(in) :: dataset_name
    integer(4), dimension(:), intent(in) :: buffer
    integer :: hdferr
    integer(hid_t) :: hdf_id
    integer(hsize_t), dimension(1) :: dims

    dims = shape(buffer)
    hdf_id = loc_id

    call h5ltmake_dataset_int_f(hdf_id, dataset_name, 1, dims, buffer, hdferr)

  end subroutine


  subroutine cd_int_8_1d(loc_id, dataset_name, buffer)
    integer, intent(in) :: loc_id
    character(len=*), intent(in) :: dataset_name
    integer(8), dimension(:), target, intent(in) :: buffer
    integer :: hdferr
    integer(hid_t) :: hdf_id, dspace_id, kind_type, dset_id
    integer(hsize_t), dimension(1) :: dims
    type(c_ptr) :: f_ptr

    dims = shape(buffer)
    hdf_id = loc_id

    ! We need to create the kind type for writing into storage,
    ! THIS NEEDS TO BE CHANGED TO NOT USE 8 AS A MAGIC NUMBER
    kind_type = h5kind_to_type(8, H5_INTEGER_KIND)

    ! Create a dataspace (aka r/w buffer)
    call h5screate_simple_f(1, dims, dspace_id, hdferr)

    ! create a dataset in the file to write to
    call h5dcreate_f(hdf_id, dataset_name, kind_type, dspace_id, dset_id, hdferr)

    ! this is dangerous(assumes array indexed from 1,1
    f_ptr = C_LOC(buffer(1))

    ! write the buffer to file
    CALL h5dwrite_f(dset_id, kind_type, f_ptr, hdferr)

    ! close both the dataset and the dataspace
    CALL h5dclose_f(dset_id, hdferr)
    CALL h5sclose_f(dspace_id, hdferr)

  end subroutine

  ! MATRIX METHODS

  subroutine cd_real_4_2d(loc_id, dataset_name, buffer)
    integer, intent(in) :: loc_id
    character(len=*), intent(in) :: dataset_name
    real(4), dimension(:,:), intent(in) :: buffer
    integer :: hdferr
    integer(hid_t) :: hdf_id
    integer(hsize_t), dimension(2) :: dims

    dims = shape(buffer)
    hdf_id = loc_id

    call h5ltmake_dataset_float_f(hdf_id, dataset_name, 2, dims, buffer, hdferr)

  end subroutine

  subroutine cd_real_8_2d(loc_id, dataset_name, buffer)
    integer, intent(in) :: loc_id
    character(len=*), intent(in) :: dataset_name
    real(8), dimension(:,:), intent(in) :: buffer
    integer :: hdferr
    integer(hid_t) :: hdf_id
    integer(hsize_t), dimension(2) :: dims

    dims = shape(buffer)
    hdf_id = loc_id

    call h5ltmake_dataset_double_f(hdf_id, dataset_name, 2, dims, buffer, hdferr)

  end subroutine


  subroutine cd_int_4_2d(loc_id, dataset_name, buffer)
    integer, intent(in) :: loc_id
    character(len=*), intent(in) :: dataset_name
    integer(4), dimension(:,:), intent(in) :: buffer
    integer :: hdferr
    integer(hid_t) :: hdf_id
    integer(hsize_t), dimension(2) :: dims

    dims = shape(buffer)
    hdf_id = loc_id

    call h5ltmake_dataset_int_f(hdf_id, dataset_name, 2, dims, buffer, hdferr)

  end subroutine

  subroutine cd_int_8_2d(loc_id, dataset_name, buffer)
    integer, intent(in) :: loc_id
    character(len=*), intent(in) :: dataset_name
    integer(8), dimension(:,:), target, intent(in) :: buffer
    integer :: hdferr
    integer(hid_t) :: hdf_id, dspace_id, kind_type, dset_id
    integer(hsize_t), dimension(2) :: dims
    type(c_ptr) :: f_ptr

    dims = shape(buffer)
    hdf_id = loc_id
    kind_type = h5kind_to_type(8,H5_INTEGER_KIND)

    call h5screate_simple_f(2, dims, dspace_id, hdferr) ! creates a read/write buffer

    call h5dcreate_f(hdf_id, dataset_name, kind_type, dspace_id, dset_id, hdferr)
    f_ptr = C_LOC(buffer(1,1))
    CALL h5dwrite_f(dset_id, kind_type, f_ptr, hdferr)
    CALL h5dclose_f(dset_id, hdferr)
    CALL h5sclose_f(dspace_id, hdferr)

  end subroutine
  
  ! END OF MATRIX METHODS

  subroutine create_dataset_str(loc_id, dataset_name, buffer)
    integer, intent(in) :: loc_id
    character(len=*), intent(in) :: dataset_name
    character(len=*), intent(in) :: buffer
    integer :: hdferr
    integer(hid_t) :: hdf_id

    hdf_id = loc_id

    call h5ltmake_dataset_string_f(hdf_id, dataset_name, buffer, hdferr)

  end subroutine

end module
