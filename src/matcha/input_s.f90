! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(input_m) input_s
  implicit none

contains
      
   module procedure num_cells
       n = self%num_cells_
   end procedure
   
  module procedure num_positions
      n = self%num_positions_
  end procedure
  
 module procedure num_dimensions
     n = self%num_dimensions_
 end procedure

 module procedure num_intervals
   n = self%num_intervals_
 end procedure

 module procedure time_step
   dt = self%time_step_
 end procedure
 
end submodule input_s
