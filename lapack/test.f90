program test

  use h5output

  implicit none

  character(len=20), parameter :: filename = "test.h5"
  character(len=10), parameter :: dataset = "powers"
  character(len=10), parameter :: dataset2 = "two-powers"
  integer :: i, j, loc_id
  double precision, dimension(100) :: powers
  double precision, dimension(1000,1000) :: two_powers

  ! initialize our data. 
  call init_hdf

  ! i => element in dataspace

  powers = (/ (2.0 ** i, i = 1,100) /)

  call create_hdf_file(filename, loc_id)

  call random_number( two_powers )

  ! create a new hdf5 file using defaults, truncating file if it exists
  call create_dataset_vec(loc_id, dataset, powers)
  call create_dataset_mat(loc_id, dataset2, two_powers)

  call close_hdf


  ! create array datatypes for file and mem



end program test








