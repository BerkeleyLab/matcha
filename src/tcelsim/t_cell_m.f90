module t_cell_m
  implicit none

  interface   
    
    module subroutine initialize_positions(x,y,z,ncells,npositions)
      implicit none
      integer, intent(in) :: ncells,npositions
      double precision, intent(out) :: x(ncells,npositions)
      double precision, intent(out) :: y(ncells,npositions)
      double precision, intent(out) :: z(ncells,npositions)
    end subroutine initialize_positions
    
  end interface

end module t_cell_m
