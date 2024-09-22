program main

  use gauss
  implicit none
  double precision, allocatable :: xn(:),wn(:)
  double precision :: xmin,xmax
  double precision :: tot
  integer :: n,i
  ! interface for integrand function
  interface
    function integrand(x) result(res)
    double precision, intent(in) :: x
    double precision :: res
    end function integrand
  end interface
  ! lower and upper limits of integration
  xmin = -1d0
  xmax = 10d0 ! 1d0, 10d0, 50d0, 100d0

  write(*,*) "Integrating:"
  write(*,*) "J_0(x)*J_1(x)"
  write(*,*) "x = [",xmin,",",xmax,"]"

  n = 4; tot = 0d0
  allocate(xn(n));  allocate(wn(n))
  call gl4(xmin,xmax,xn,wn)
  do i = 1,n
    tot = tot + integrand(xn(i)) * wn(i)
  enddo
  deallocate(xn);  deallocate(wn)
  write(*,*) "GL4 :",tot

  n = 8; tot = 0d0
  allocate(xn(n));  allocate(wn(n))
  call gl8(xmin,xmax,xn,wn)
  do i = 1,n
    tot = tot + integrand(xn(i)) * wn(i)
  enddo
  deallocate(xn);  deallocate(wn)
  write(*,*) "GL8 :",tot

  n = 16; tot = 0d0
  allocate(xn(n));  allocate(wn(n))
  call gl16(xmin,xmax,xn,wn)
  do i = 1,n
    tot = tot + integrand(xn(i)) * wn(i)
  enddo
  deallocate(xn);  deallocate(wn)
  write(*,*) "GL16:",tot

  n = 32; tot = 0d0
  allocate(xn(n));  allocate(wn(n))
  call gl32(xmin,xmax,xn,wn)
  do i = 1,n
    tot = tot + integrand(xn(i)) * wn(i)
  enddo
  deallocate(xn);  deallocate(wn)
  write(*,*) "GL32:",tot

  n = 64; tot = 0d0
  allocate(xn(n));  allocate(wn(n))
  call gl64(xmin,xmax,xn,wn)
  do i = 1,n
    tot = tot + integrand(xn(i)) * wn(i)
  enddo
  deallocate(xn);  deallocate(wn)
  write(*,*) "GL64:",tot

  ! actual result
  write(*,*) "real:",(bessel_j0(xmin)**2-bessel_j0(xmax)**2)/2d0

end program main



function integrand(x) result(res)
  implicit none
  double precision, intent(in) :: x
  double precision :: res
  res = bessel_j0(x)*bessel_j1(x)
  return
end function integrand
