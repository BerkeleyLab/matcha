module distribution_m
  implicit none
  
  private
  public :: distribution_t
  
  type distribution_t
    private
    double precision, allocatable, dimension(:) :: vel_, cumulative_distribution_
  contains
    procedure :: cumulative_distribution
    procedure :: velocities
  end type  

  interface distribution_t
  
   pure module function construct(sample_distribution) result(distribution)
      implicit none
      double precision, intent(in) :: sample_distribution(:)
      type(distribution_t) distribution
    end function
    
  end interface
  
  interface
  
    pure module function cumulative_distribution(self) result(my_cumulative_distribution)
      implicit none
      class(distribution_t), intent(in) :: self
      double precision, allocatable :: my_cumulative_distribution(:)
    end function
    
    pure module function velocities(self, speeds, directions) result(my_velocities)
      !! Return the t_cell_collection_t object's velocity vectors
      implicit none
      class(distribution_t), intent(in) :: self
      double precision, intent(in) :: speeds(:,:), directions(:,:,:)
      double precision, allocatable :: my_velocities(:,:,:)
    end function
    
  end interface
  
end module distribution_m
