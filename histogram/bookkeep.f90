module bookkeep

  use, intrinsic :: iso_fortran_env, only : wp => real64
  use, intrinsic :: iso_fortran_env, only : ip => int32
  use histogram
  implicit none
  private
  ! declare dynamic list of histograms
  type(hist), dimension(:), allocatable :: book
  integer(ip) :: booksize
  public :: initbook,newbook,fillbook,printbook

contains

  ! initialize histogram book
  subroutine initbook()
  allocate(book(0))
  booksize = 0
  end subroutine initbook

  ! new historgam(book)
  subroutine newbook(i,l,t,n,hmin,hmax)
  integer(ip), intent(in) :: i,n
  logical, intent(in) :: l
  character(len=*), intent(in) :: t
  real(wp), intent(in) :: hmin,hmax
  type(hist) :: htemp
  call htemp%seth(i,l,t,n,hmin,hmax)
  book = [book,htemp]
  booksize = booksize + 1
  end subroutine newbook

  ! fill historam(book)
  subroutine fillbook(i,x,y)
  integer(ip), intent(in) :: i
  real(wp), intent(in) :: x,y
  integer(ip) :: j
  do j = 1, booksize
    if( book(j)%geti() .eq. i ) then
      call book(j)%fillh(x,y);  exit
    endif
  enddo
  end subroutine fillbook

  ! print all histograms in book
  subroutine printbook(u)
  integer, intent(in) :: u
  integer :: i
  do i = 1, booksize
    call book(i)%printh(u)
  enddo
  end subroutine printbook

end module bookkeep
