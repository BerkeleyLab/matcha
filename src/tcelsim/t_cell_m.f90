module t_cell_m
  implicit none

  interface   
    
    module subroutine initialize_positions(x,y,z,random_number_table_positions,ncells,npositions,ndim)
      implicit none
      integer, intent(in) :: ncells,npositions,ndim
      double precision, intent(in)  :: random_number_table_positions(ncells,ndim)
      double precision, intent(out) :: x(ncells,npositions)
      double precision, intent(out) :: y(ncells,npositions)
      double precision, intent(out) :: z(ncells,npositions)
    end subroutine initialize_positions
    
  end interface

end module t_cell_m
