! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module input_m
  implicit none
  
  private
  public :: input_t
  
  type input_t
    private
    integer :: num_cells_ = 6000, num_positions_ = 6000, num_dimensions_ = 3
    integer :: num_intervals_ = 4, num_angle_intervals_ = 4, ngrid_ = 50
    
    double precision :: time_step_ = 0.1d0
    double precision :: grid_begin_ = -500.d0, grid_end_ = 600.d0, cytokine_ = 1.d-6, gfac_ = 1.d-16
    double precision :: alpha_ = 1.d-10
  contains
    procedure :: num_cells
    procedure :: num_positions
    procedure :: num_dimensions
    procedure :: num_intervals
    procedure :: num_angle_intervals
    procedure :: ngrid
    procedure :: time_step
    procedure :: grid_begin
    procedure :: grid_end
    procedure :: cytokine
    procedure :: gfac
    procedure :: alpha
    procedure :: sample_distribution
    procedure :: sample_angle_distribution
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

    pure module function num_angle_intervals(self) result(n)
      implicit none
      class(input_t), intent(in) :: self
      integer n
    end function 
    

    pure module function ngrid(self) result(n)
      implicit none
      class(input_t), intent(in) :: self
      integer n
    end function 
    
    pure module function time_step(self) result(dt)
      implicit none
      class(input_t), intent(in) :: self
      double precision dt
    end function time_step

    pure module function grid_begin(self) result(gg)
      implicit none
      class(input_t), intent(in) :: self
      double precision gg
    end function grid_begin

    pure module function grid_end(self) result(gg)
      implicit none
      class(input_t), intent(in) :: self
      double precision gg
    end function grid_end

    pure module function cytokine(self) result(gg)
      implicit none
      class(input_t), intent(in) :: self
      double precision gg
    end function cytokine

    pure module function gfac(self) result(gg)
      implicit none
      class(input_t), intent(in) :: self
      double precision gg
    end function gfac
    
    pure module function alpha(self) result(gg)
      implicit none
      class(input_t), intent(in) :: self
      double precision gg
    end function alpha
    

    pure module function sample_distribution(self) result(empirical_distribution)
      implicit none
      class(input_t), intent(in) :: self
      double precision, allocatable :: empirical_distribution(:,:)
    end function sample_distribution    
    
    pure module function sample_angle_distribution(self) result(empirical_angle_distribution)
      implicit none
      class(input_t), intent(in) :: self
      double precision, allocatable :: empirical_angle_distribution(:,:)
    end function sample_angle_distribution    


 end interface
  
end module input_m
