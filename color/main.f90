program main

  use, intrinsic :: iso_fortran_env, only: stdout=>output_unit
  use color
  implicit none
  ! test phrase
  character(len=*), parameter :: phrase = '1234 abcd ABCD +-*/'
  integer(1) :: u,code,i,j

  ! print ASCII table
  u = stdout
  write(stdout,*) 'ASCII character table:',new_line('a')
  write(stdout,*) '     0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15'
  do i = 2,7
    write(u,'(i3.3,a)',advance='no') i*16,'+'
    do j = 0,15
      write(u,'(a3)',advance='no') achar(i*16 + j)
    enddo
    write(u,*)
  enddo
  write(u,*)
 
  ! print test phrase in different colors 
  write(u,*) 'color code table:',new_line('a')
  do code = 30, 37
    write(u,*) 'code:',code,', sample: ',col(phrase, code)
  enddo
  write(u,*)
  do code = 90, 97
    write(u,*) 'code:',code,', sample: ',col(phrase, code)
  enddo
  write(u,*)

  ! print a sentence using different colors
  write(u,*)  &
    col("the sentence ",cblue), &
    col("can have ",cmagenta), &
    col("different ",ccyan), &
    col("colors.",cwhite)
  write(u,*)  &
    col("the sentence ",bblack), &
    col("can have ",bred), &
    col("different ",bgreen), &
    col("colors.",byellow)

end program main
