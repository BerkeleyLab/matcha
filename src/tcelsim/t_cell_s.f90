submodule(t_cell_m) t_cell_s
  implicit none

contains
  
  module procedure initialize_positions

    !     Local variables
    integer i
    double precision rr1,rr2,rr3
    double precision scaling_factor

    scaling_factor = 100.d0
        
    !     Assign initial positions to T cells randomly in a [100x100x100] grid
    do i = 1,ncells
       call random_number(rr1)
       call random_number(rr2)
       call random_number(rr3)
       x(i,1) = rr1*scaling_factor
       y(i,1) = rr2*scaling_factor
       z(i,1) = rr3*scaling_factor
    end do

  end procedure initialize_positions

end submodule t_cell_s
