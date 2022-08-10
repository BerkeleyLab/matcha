! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module input_m
  implicit none
  
  private
  public :: input_t
  
  type input_t
    private
    integer :: num_cells_ = 1000, num_positions_ = 1000, num_dimensions_ = 3, num_intervals_ = 4
    double precision :: time_step_ = 0.1D0
  contains
    procedure :: num_cells
    procedure :: num_positions
    procedure :: num_dimensions
    procedure :: num_intervals
    procedure :: time_step
    procedure :: sample_distribution
  end type
  
  interface
    
    pure module function num_cells(self) result(n)
      implicit none
      class(input_t), intent(in) :: self
      integer n
    end function

    pure module function num_positions(self) result(n)
      implicit none
      class(input_t), intent(in) :: self
      integer n
    end function
      
    pure module function num_dimensions(self) result(n)
      implicit none
      class(input_t), intent(in) :: self
      integer n
    end function
    
    pure module function num_intervals(self) result(n)
      implicit none
      class(input_t), intent(in) :: self
      integer n
    end function 
    
    pure module function time_step(self) result(dt)
      implicit none
      class(input_t), intent(in) :: self
      double precision dt
    end function time_step
    
    pure module function sample_distribution(self) result(empirical_distribution)
      implicit none
      class(input_t), intent(in) :: self
      double precision, allocatable :: empirical_distribution(:,:)
    end function sample_distribution    
    
  end interface
  
end module input_m
