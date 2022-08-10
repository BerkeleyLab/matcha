! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(t_cell_collection_m) t_cell_collection_s
 
#ifdef USE_CAFFEINE
   use caffeine_assert_m, only : assert
#else
   use assert_m, only : assert
#endif
 
  implicit none
  
contains
      
  pure module function construct(positions, time) result(t_cell_collection)
    double precision, intent(in) :: positions(:,:), time
    type(t_cell_collection_t) t_cell_collection
    t_cell_collection%positions_ = positions
    t_cell_collection%time_ = time
  end function 
  
  pure module function positions(self) result(my_positions)
    class(t_cell_collection_t), intent(in) :: self
    double precision, allocatable :: my_positions(:,:)
    call assert(allocated(self%positions_), "t_cell_collection_t%positions: allocated(positions_)")
    my_positions = self%positions_
  end function
  
  elemental module function time(self) result(my_time)
    class(t_cell_collection_t), intent(in) :: self
    double precision my_time
    my_time = self%time_
  end function
    
end submodule t_cell_collection_s
