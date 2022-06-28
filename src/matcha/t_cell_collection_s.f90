submodule(t_cell_collection_m) t_cell_collection_s
  implicit none
  
contains
      
  module procedure construct
    t_cell_collection%positions_ = positions
    t_cell_collection%time_ = time
  end procedure 
  
  module procedure positions
    my_positions = self%positions_
  end procedure
  
  module procedure time
    my_time = self%time_
  end procedure
    
end submodule t_cell_collection_s
