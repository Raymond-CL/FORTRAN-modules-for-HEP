! === optical glauber module ===
! nuclear data from:
! H. De Vries, C. W. De Jager and C. De Vries
! Nuclear charge and magnetization density distribution parameters -
! from elastic electron scattering
! Atomic Data and Nuclear Data Tables 36, 3 (1987) 495
! although the data are extracted from electron scatterings
! which means c and z is related to the nuclear charge
! and that rho0 should satisfy the normalization to the nuclear charge
! we set rho0 such that it satisfy the mass normalization
module glauber_opt

  use gauss
  use, intrinsic :: iso_fortran_env, only: wp=>real64
  implicit none
  private
  ! nuclear mass number, neutron number, and atomic number
  integer :: nucA,nucN,nucZ
  ! nuclear radius, skin width, deformity, and core density
  real(wp) :: z,c,w,rho0
  ! gaussian quadrature parameters
  real(wp), parameter :: zmax=20d0
  integer, parameter :: GLn=64
  ! public namespace
  public :: setgeo,rho,TA,TAB

contains

  ! subroutine to preset nuclear data
  subroutine setgeo(A)
  integer, intent(in) :: A
  if(A.eq.238) then   ! U-238
    nucA=A;   nucZ=92;    nucN=nucA-nucZ
    c=6.8054d0; z=0.605d0;  w=0d0
    rho0=0.167228d0
  elseif(A.eq.208) then   ! Pb-208: 305.002d0
    nucA=A;   nucZ=82;    nucN=nucA-nucZ
    c=6.62d0;   z=0.546d0;  w=0d0
    rho0=0.160391d0
  elseif(A.eq.207) then   ! Pb-207: 302.077d0
    nucA=A;   nucZ=82;    nucN=nucA-nucZ
    c=6.62d0;   z=0.546d0;  w=0d0
    rho0=0.15962d0
  elseif(A.eq.197) then   ! Au-197
    nucA=A;   nucZ=79;    nucN=nucA-nucZ
    c=6.38d0;   z=0.535d0;  w=0d0
    rho0=0.169346d0
  elseif(A.eq.96) then    ! Zr-96
    nucA=A;   nucZ=40;    nucN=nucA-nucZ
    c=4.503d0;  z=2.602d0;  w=0.341d0
    rho0=0.1686d0
  else  ! mass parametrization in EKRT NPB 323 (1989) 57
    nucA=A;   nucZ=0;    nucN=0
    c=1.12d0*A**(1d0/3d0) - 0.86d0*A**(-1d0/3d0)
    z=0.54d0;   w=0d0;  
    rho0=0.16d0
  endif
  end subroutine setgeo

  ! nuclear density function (with Cartesian coordinate as input)
  function rho(xin,yin,zin) result(res)
  real(wp), intent(in) :: xin,yin,zin
  real(wp) :: res
  real(wp) :: r
  r = sqrt(xin*xin+yin*yin+zin*zin)
  if(nucA.eq.96) then
    res = rho0*(1d0+w*r*r/c/c)/(1d0+exp((r*r-c*c)/z/z))
  else
    res = rho0/(1d0+exp((r-c)/z))
  endif
  return
  end function rho

  ! thickness function
  function TA(xin,yin) result(res)
  real(wp), intent(in) :: xin,yin
  real(wp) :: res
  real(wp), dimension(1:GLn) :: zx,zw
  integer :: i
  call GL64(-zmax,+zmax,zx,zw)
  res = 0d0
  do i = 1,GLn
    res = res + rho(xin,yin,zx(i)) * zw(i)
  enddo
  return
  end function TA

  !overlap function
  function TAB(xin,yin,bin) result(res)
  real(wp), intent(in) :: xin,yin,bin
  real(wp) :: res
  real(wp), dimension(1:GLn) :: zx,zw
  integer :: i
  real(wp) :: xa,xb,ta,tb
  call GL64(-zmax,+zmax,zx,zw)
  ta = 0d0;   tb = 0d0
  do i = 1,GLn
    ! reaction plane along x-axis
    xa = xin - bin/2d0
    xb = xin + bin/2d0
    ta = ta + rho(xa,yin,zx(i)) * zw(i)
    tb = tb + rho(xb,yin,zx(i)) * zw(i)
  enddo
  res = ta * tb
  end function TAB

end module glauber_opt
