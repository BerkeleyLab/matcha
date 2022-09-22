module do_concurrent_m
  implicit none
  private
  public :: do_concurrent_sampled_speeds, do_concurrent_my_velocities
  
  interface
    
    pure module function do_concurrent_sampled_speeds(speeds, vel, cumulative_distribution) result(sampled_speeds)
      implicit none
      double precision, intent(in) :: speeds(:,:), vel(:), cumulative_distribution(:)
      double precision, allocatable :: sampled_speeds(:,:)
    end function
    
    pure module function do_concurrent_my_velocities(speeds, directions, sampled_speeds) result(my_velocities)
      implicit none
      double precision, intent(in) :: speeds(:,:), directions(:,:,:), sampled_speeds(:,:)
      double precision, allocatable :: my_velocities(:,:,:)
    end function
      

  end interface
  
end module do_concurrent_m
