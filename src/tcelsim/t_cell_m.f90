module t_cell_m
  implicit none

  interface   
    
    module subroutine initialize_positions(x,y,z,random_positions)
      implicit none
      double precision, intent(in)  :: random_positions(:,:)
      double precision, intent(out) :: x(:), y(:), z(:)
    end subroutine
    
  end interface

end module t_cell_m
