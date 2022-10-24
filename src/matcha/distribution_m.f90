! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module distribution_m
  implicit none
  
  private
  public :: distribution_t
  
  type distribution_t
    private
    double precision, allocatable, dimension(:) :: vel_,cumulative_distribution_,angle_,cumulative_angle_distribution_
  contains
    procedure :: cumulative_distribution
    procedure :: cumulative_angle_distribution
    procedure :: velocities
  end type  

  interface distribution_t
  
   pure module function construct(sample_distribution,sample_angle_distribution) result(distribution)
      implicit none
      double precision, intent(in) :: sample_distribution(:,:),sample_angle_distribution(:,:)
      type(distribution_t) distribution
    end function 

  end interface
  
  interface
  
    pure module function cumulative_distribution(self) result(my_cumulative_distribution)
      implicit none
      class(distribution_t), intent(in) :: self
      double precision, allocatable :: my_cumulative_distribution(:)
    end function cumulative_distribution
    
    pure module function cumulative_angle_distribution(self) result(my_cumulative_angle_distribution)
      implicit none
      class(distribution_t), intent(in) :: self
      double precision, allocatable :: my_cumulative_angle_distribution(:)
    end function cumulative_angle_distribution

    
    pure module function velocities(self, speeds, angles, directions) result(my_velocities)
      !! Return the t_cell_collection_t object's velocity vectors
      implicit none
      class(distribution_t), intent(in) :: self
      double precision, intent(in) :: speeds(:,:),angles(:,:),directions(:,:,:)
      double precision, allocatable :: my_velocities(:,:,:)
    end function velocities

  end interface

end module distribution_m
