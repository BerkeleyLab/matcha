module t_cell_m
  implicit none

contains
  
  subroutine initialize_positions(x,y,z,ncells,npositions)
        
    implicit none
    integer, intent(in) :: ncells,npositions
    double precision, intent(out) :: x(ncells,npositions)
    double precision, intent(out) :: y(ncells,npositions)
    double precision, intent(out) :: z(ncells,npositions)

    !     Local variables
    integer i
    double precision rr1,rr2,rr3
        
    !     Assign initial positions to T cells randomly in a [100x100x100] grid
    do i = 1,ncells
       call random_number(rr1)
       call random_number(rr2)
       call random_number(rr3)
       x(i,1) = rr1*100.d0
       y(i,1) = rr2*100.d0
       z(i,1) = rr3*100.d0
    end do

  end subroutine initialize_positions

end module t_cell_m
