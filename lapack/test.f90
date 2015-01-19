program test

  use h5output

  implicit none

  character(len=20), parameter :: filename = "test.h5"
  integer :: i, j, loc_id
  double precision, dimension(100) :: powers
  double precision, dimension(1000,1000) :: two_powers
  integer, dimension(500) :: three_powers
  integer(8), dimension(40,20) :: eight
  integer(8), dimension(50) :: one_dimension
  complex(8), dimension(30) :: c

  ! initialize our data. 
  call init_hdf

  ! i => element in dataspace

  powers = (/ (2.0 ** i, i = 1,100) /)
  three_powers = (/ (i ** 3, i = 1, 500) /)
  eight = reshape((/ (i * 2, i = 1,40*20)/), shape(eight))
  one_dimension = (/ (i + 5, i = 1, 50) /)
  c = (/ (sqrt(cmplx(i,i-1)), i = 1,30) /)

  call create_hdf_file(filename, loc_id)

  call random_number( two_powers )

  print *, "Finished setting data"
  print *, c
  call flush

  ! create a new hdf5 file using defaults, truncating file if it exists
  call create_dataset_vec(loc_id, "powers", powers)

  print *, "powers done"
  call flush
  call create_dataset_vec(loc_id, "complex", c)
  call create_dataset_vec(loc_id, "one-dimension", one_dimension)
  call create_dataset_vec(loc_id, "three-powers", three_powers)
  call create_dataset_mat(loc_id, "two-powers", two_powers)
  call create_dataset_mat(loc_id, "eight", eight)

  call close_hdf


  ! create array datatypes for file and mem



end program test








