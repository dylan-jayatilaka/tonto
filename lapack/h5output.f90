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
  end interface

  interface create_dataset_mat
    module procedure cd_real_4_2d
    module procedure cd_real_8_2d
    module procedure cd_int_4_2d
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
 
end module
