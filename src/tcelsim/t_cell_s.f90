submodule(t_cell_m) t_cell_s
  implicit none

contains
  
  module procedure initialize_positions

    !     Local variables
    integer i
    double precision, parameter :: scaling_factor  = 100.

        
    !     Assign initial positions to T cells randomly in a [100x100x100] grid
    do i = 1,ncells
       x(i,1) = random_number_table_positions(i,1)*scaling_factor
       y(i,1) = random_number_table_positions(i,2)*scaling_factor
       z(i,1) = random_number_table_positions(i,3)*scaling_factor
    end do

  end procedure initialize_positions

end submodule t_cell_s
