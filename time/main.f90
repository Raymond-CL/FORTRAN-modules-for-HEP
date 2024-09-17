program main
  use time
  implicit none
  real, allocatable :: matA(:,:), matB(:,:), matC(:,:)
  integer :: ndim

  call time_start

  ndim = 100
  allocate(matA(ndim,ndim))
  allocate(matB(ndim,ndim))
  allocate(matC(ndim,ndim))

  call random_number(matA)
  call random_number(matB)

  matC = matmul(matA,matB)

  !write(*,*) matA
  !write(*,*) matB
  !write(*,*) matC

  call time_stop

  call print_time
end program main
