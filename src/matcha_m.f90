! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module matcha_m 
  use t_cell_collection_m, only : t_cell_collection_t
  use distribution_m, only : distribution_t
  use input_m, only : input_t
  use output_m, only : output_t
  use data_partition_m, only : data_partition_t
  
  implicit none

  interface

    module function matcha(input) result(history)
      implicit none
      type(input_t), intent(in) :: input
      type(t_cell_collection_t), allocatable :: history(:)
    end function

  end interface
  
end module
