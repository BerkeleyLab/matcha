! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module output_m
  !! Output data abstraction
  use input_m, only : input_t
  use t_cell_collection_m, only : t_cell_collection_t
  implicit none
  
  private
  public :: output_t
  
  type output_t
    !! Encapsulate an input/result pair
    private
    type(input_t) input_
    type(t_cell_collection_t), allocatable :: history_(:)
  contains
    procedure :: simulated_distribution
    procedure :: my_num_cells
  end type
  
  interface output_t

    pure module function construct(input, history) result(output)
      !! Construct a new output_t object
      implicit none
      type(input_t), intent(in) :: input
      type(t_cell_collection_t), intent(in) :: history(:)
      type(output_t) :: output
    end function

  end interface

  interface
    
    pure module function simulated_distribution(self) result(output_distribution)
      !! The result is a histogram calculated from the simulation output
      implicit none
      class(output_t), intent(in) :: self
      double precision, allocatable :: output_distribution(:,:)
    end function
    
    pure module function my_num_cells(self) result(num_cells)
      implicit none
      class(output_t), intent(in) :: self
      integer num_cells
    end function
    
  end interface
  
end module output_m
