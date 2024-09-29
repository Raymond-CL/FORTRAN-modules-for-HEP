program main

  use, intrinsic :: iso_fortran_env, only : stdout => output_unit
  use bookkeep
  implicit none
  integer, parameter :: n = 1000000
  integer :: i
  double precision, parameter :: twoPI = 8d0*atan(1d0)
  double precision :: r1,r2,z1,z2
  double precision :: weight,mu,sigma,mag

  ! each point has weight = 1/n
  weight = real(1d0/n,8)

  ! initialize book
  call initbook()
  ! create a few histograms
  call newbook(1, .false., "mu=+1, sigma=1", 40, -5d0, 5d0)
  call newbook(2, .false., "mu=-1, sigma=1", 40, -5d0, 5d0)
  call newbook(5, .false., "mu=+2, sigma=2", 40, -5d0, 5d0)
  call newbook(6, .false., "mu=-2, sigma=2", 40, -5d0, 5d0)
  
  ! set mu and sig for normal distribution
  mu = 1d0
  sigma = 1d0
  ! loop to fill in histogram
  do i = 1, n
    call random_number(r1)
    call random_number(r2)
    ! standard box-mueller method
    mag = sigma * sqrt(-2d0*log(r1))
    z1 = mag*cos(twoPI*r2) + mu
    z2 = mag*sin(twoPI*r2) - mu
    call fillbook(1, z1, weight)
    call fillbook(2, z2, weight)
  enddo

  ! do the same for different settings
  mu = 2d0
  sigma = 2d0
  do i = 1, n
    call random_number(r1)
    call random_number(r2)
    mag = sigma * sqrt(-2d0*log(r1))
    z1 = mag*cos(twoPI*r2) + mu
    z2 = mag*sin(twoPI*r2) - mu
    call fillbook(5, z1, weight)
    call fillbook(6, z2, weight)
  enddo

  ! print histogram to either file, or terminal
  open(unit=22,file="output.dat",status="unknown")
  call printbook(22)
  close(22)

end program main
