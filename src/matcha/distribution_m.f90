module distribution_m
  implicit none
  
  private
  public :: distribution_t
  
  type distribution_t
    private
    double precision, allocatable, dimension(:) :: vel_, cumulative_distribution_
  contains
    procedure :: vel
    procedure :: cumulative_distribution
  end type  

  interface distribution_t
  
   module function construct(sample_distribution) result(distribution)
      implicit none
      double precision, intent(in) :: sample_distribution(:)
      type(distribution_t) distribution
    end function
    
  end interface
  
  interface
  
    pure module function vel(self) result(my_vel)
      implicit none
      class(distribution_t), intent(in) :: self
      double precision, allocatable :: my_vel(:)
    end function
    
    pure module function cumulative_distribution(self) result(my_cumulative_distribution)
      implicit none
      class(distribution_t), intent(in) :: self
      double precision, allocatable :: my_cumulative_distribution(:)
    end function
  end interface
  
end module distribution_m
