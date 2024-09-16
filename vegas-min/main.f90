! define a variable in global scope
module radius
  implicit none
  double precision :: rad
end module radius



! main program
program main
  use vint
  use radius
  implicit none
  ! vegas necessary variables
  integer :: init,itmax,ncall,ndim,nprn
  real(wp) :: avgi,chi2a,sd
  real(wp), dimension(20) :: region
  ! set coefficients
  ndim = 3
  rad = 1.0
  ncall = 1000000
  itmax = 1
  nprn = -1
  region(1:ndim) = 0.0
  region(1+ndim:2*ndim) = rad
  ! warm-up the vegas adaptive grid
  init = -1
  call vegas(region(1:2*ndim),fxn,init,ncall,10*itmax,nprn,avgi,sd,chi2a)
  ! actual vegas run
  init = +1
  call vegas(region(1:2*ndim),fxn,init,10*ncall,itmax,nprn,avgi,sd,chi2a)
  ! integrated result (multiplied by symmetry factor)
  write(*,*) "integrated result :",avgi * 2**ndim
  ! actual result
  write(*,*) "theoretical result:",PI**(ndim/2.0) / gamma(ndim/2.0+1.0) * rad**ndim  
end program main



! integrand function
function fxn(pt,wgt)
  use radius
  implicit none
  double precision, dimension(:), intent(in) :: pt
  double precision, intent(in) :: wgt
  double precision :: fxn
  fxn = 0.0
  ! using rejection sampling (Euclidean norm)
  if( sum(pt(:)**2) .lt. rad**2 ) fxn = 1.0
  return
end function fxn
