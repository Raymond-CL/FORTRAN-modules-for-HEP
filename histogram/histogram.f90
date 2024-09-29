module histogram

  use, intrinsic :: iso_fortran_env, only : wp => real64
  use, intrinsic :: iso_fortran_env, only : ip => int32   !max:32767
  use, intrinsic :: iso_fortran_env, only : ip2 => int64  !max:9E19
  implicit none
  private
  ! declare hist class
  type :: hist
    private
    integer(ip) :: ind,histsize
    integer(ip2) :: hitcount,misscount
    logical :: logplot
    character(len=50) :: title
    real(wp) :: histmin,histmax,histbin
    real(wp) :: total
    real(wp), allocatable :: histdata(:)
  contains
    procedure, public :: geti => getind
    procedure, public :: seth => sethist
    procedure, public :: fillh => fillhist
    procedure, public :: printh => printhist
  end type hist
  ! every everything except the hist class and its module procedures
  public :: hist

contains

  ! function to retrieve index of histogram
  function getind(this) result(i)
  class(hist), intent(in) :: this
  integer(ip) :: i 
  i = this%ind
  end function getind

  ! initial setting of the histogram
  subroutine sethist(this,i,islog,t,n,hmin,hmax)
  class(hist), intent(inout) :: this
  integer(ip), intent(in) :: i,n
  logical, intent(in) :: islog
  character(len=*), intent(in) :: t
  real(wp), intent(in) :: hmin,hmax
  integer(ip) :: err
  this%ind = i
  this%logplot = islog
  this%title = t
  this%histsize = n
  allocate(this%histdata(n),stat=err)
  if(err.ne.0) then
    write(*,*) "memory allocation error!",err
    call exit(1)
  endif
  this%histdata = 0d0
  if(hmin.lt.hmax) then
    this%histmin = hmin;  this%histmax = hmax
  else
    this%histmin = hmax;  this%histmax = hmin
  endif
  if(this%logplot) then
    if(this%histmin.le.0d0) then
      write(*,*) "histmin<=0 for logplot, setting to 1E-10."
      this%histmin = 1d-10
    endif
    this%histbin = log(this%histmax/this%histmin)/this%histsize
  else
    this%histbin = (this%histmax-this%histmin)/this%histsize
  endif
  this%hitcount = 0
  this%misscount = 0
  this%total = 0d0
  end subroutine sethist

  ! entry filling
  subroutine fillhist(this,x,y)
  class(hist), intent(inout) :: this
  real(wp), intent(in) :: x,y
  integer(ip) :: i
  if(x.lt.this%histmin .or. x.ge.this%histmax) then
    this%misscount = this%misscount + 1
  else
    if(this%logplot) then
      i = floor( log(x/this%histmin)/this%histbin + 1 )
    else
      i = floor( (x-this%histmin)/this%histbin + 1 )
    endif
    this%histdata(i) = this%histdata(i) + y
    this%hitcount = this%hitcount + 1
  endif
  end subroutine fillhist

  ! print histogram
  subroutine printhist(this,u)
  class(hist), intent(inout) :: this
  integer, intent(in) :: u
  integer(ip) :: i
  real(wp) :: histL,histR,histM,eff
  character(len=*), parameter :: tab=char(9)
  write(u,'(a,i5)') "histogram index:",this%ind
  write(u,*) "title :",tab,trim(this%title)
  write(u,*) "hits  :",this%hitcount
  write(u,*) "misses:",this%misscount
  eff = real(this%hitcount*100d0/(this%hitcount+this%misscount))
  if(this%hitcount .eq. 0) eff = 0d0
  write(u,'(a,f10.2,a)') "efficiency:",real(eff),"%"
  write(u,*) " |---bin-left---|---bin-mid----|---bin-right--|---result-----|"
  do i = 1, this%histsize
    if(this%logplot) then
      histL = this%histmin + (i-1)*this%histbin
      histR = this%histmin + i*this%histbin
      histM = (histL+histR)/2d0
      write(u,'(4(es15.4))') histL,histM,histR,this%histdata(i)/this%histbin
    else
      histL = this%histmin + (i-1)*this%histbin
      histR = this%histmin + i*this%histbin
      histM = (histL+histR)/2d0
      write(u,'(4(es15.4))') histL,histM,histR,this%histdata(i)/this%histbin
    endif
    this%total = this%total + this%histdata(i)
  enddo
  write(u,'(a,a,es15.4,a)') "integrated total:",tab,real(this%total),new_line("a")
  end subroutine printhist

end module histogram
