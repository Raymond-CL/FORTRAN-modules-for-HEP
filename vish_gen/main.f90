program main
  use vish_gen
  implicit none
  integer :: u
  ! read hydro data file
  call Hydro_Set
  ! generate output table
  u = 20
  open(u,file=ofile,status="replace")
  call Gen_Table(u)
  close(u)
end program main
