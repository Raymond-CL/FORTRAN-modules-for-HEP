module radius
  implicit none
  double precision :: rad
end module radius



program main
  use vint  ! uses vegas integration module
  use radius
  implicit none
  double precision, parameter :: PI = 4d0*atan(1d0)
  ! set vegas parameters
  ndim = 3
  rad = 1.0
  ncall = 1000000
  itmax = 1
  nprn = -1
  region(1:ndim) = 0.0
  region(1+ndim:2*ndim) = rad
  ! warm-up and run vegas integration
  init = -1
  call vegas(region(1:2*ndim),fxn,init,ncall,10*itmax,nprn,avgi,sd,chi2a)
  init = +1
  call vegas(region(1:2*ndim),fxn,init,10*ncall,itmax,nprn,avgi,sd,chi2a)
  write(*,*) "integrated result :",avgi * 2**ndim
  write(*,*) "theoretical result:",PI**(ndim/2.0) / gamma(ndim/2.0+1.0) * rad**ndim  
end program main



function fxn(pt,wgt)
  use radius
  implicit none
  double precision, dimension(:), intent(in) :: pt
  double precision, intent(in) :: wgt
  double precision :: fxn
  fxn = 0.0
  if( sum(pt(:)**2) .lt. rad**2 ) fxn = 1.0
  return
end function fxn
