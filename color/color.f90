module color
  
  implicit none
  !private
  ! some predefined color codes with common names
  ! change to private if you don't want to pollute the namespace
  integer(1) :: cblack,cred,cgreen,cyellow,cblue,cmagenta,ccyan,cwhite
  integer(1) :: bblack,bred,bgreen,byellow,bblue,bmagenta,bcyan,bwhite
  parameter( &
    cblack    = 30, &
    cred      = 31, &
    cgreen    = 32, &
    cyellow   = 33, &
    cblue     = 34, &
    cmagenta  = 35, &
    ccyan     = 36, &
    cwhite    = 37 )
  parameter( &
    bblack    = 90, &
    bred      = 91, &
    bgreen    = 92, &
    byellow   = 93, &
    bblue     = 94, &
    bmagenta  = 95, &
    bcyan     = 96, &
    bwhite    = 97 )
  public :: col

contains

  ! return the str colored by code without printing it out
  function col(str, code) result(res)
  character(len=*), intent(in) :: str
  integer(1), intent(in) :: code
  character(len=:), allocatable :: res
  character(len=2) :: cs
  write(cs,'(i2.2)') code
  res = achar(27) // '[' // cs // 'm' // str // achar(27) // '[0m'
  return
  end function

end module color
