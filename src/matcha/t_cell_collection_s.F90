! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

submodule(t_cell_collection_m) t_cell_collection_s
  use assert_m
  use iso_c_binding, only : c_loc
  use assert_m, only : assert
  implicit none
  
contains
      
  module procedure construct
    t_cell_collection%positions_ = positions
    t_cell_collection%time_ = time
  end procedure 
  
  module procedure positions
    call_assert(allocated(self%positions_))
    my_positions = self%positions_
  end procedure
  
  module procedure time
    my_time = self%time_
  end procedure

  module procedure construct_bind_C
    t_cell_collection_bind_C%positions_ptr = c_loc(t_cell_collection%positions_)
    t_cell_collection_bind_C%positions_shape = shape(t_cell_collection%positions_)
    t_cell_collection_bind_C%time = t_cell_collection%time_
  end procedure
    
end submodule t_cell_collection_s
