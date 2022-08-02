! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module output_m
  use input_m, only : input_t
  use t_cell_collection_m, only : t_cell_collection_t
  implicit none
  
  private
  public :: output_t
  
  type output_t
    private
    type(input_t) input_
    type(t_cell_collection_t), allocatable :: history_(:)
  contains
    procedure :: simulated_distribution
  end type
  
  interface output_t

    pure module function construct(input, history) result(output)
      implicit none
      type(input_t), intent(in) :: input
      type(t_cell_collection_t), intent(in) :: history(:)
      type(output_t) :: output
    end function

  end interface

  interface
    
    pure module function simulated_distribution(self) result(output_distribution)
      implicit none
      class(output_t), intent(in) :: self
      double precision, allocatable :: output_distribution(:,:)
    end function
    
  end interface
  
end module output_m
