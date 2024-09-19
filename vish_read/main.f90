program main
  use vish_read
  implicit none
  double precision :: x,y,th
  integer :: i
  ! read in data from file
  call Read_data
  ! test interpolation
  x = 0d0
  y = 0d0
  do i = -10,30
    th = i * 2d0*3.1416d0/20
    write(*,*) th, get_val(x,y,th)
  enddo
end program main
