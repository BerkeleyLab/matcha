module do_concurrent_m
  use iso_c_binding, only : c_double
  implicit none
  private
  public :: do_concurrent_sampled_speeds, do_concurrent_my_velocities
  
  interface
    
    pure module subroutine do_concurrent_sampled_speeds(speeds, vel, cumulative_distribution, sampled_speeds) bind(C)
      implicit none
      real(c_double), intent(in) :: speeds(:,:), vel(:), cumulative_distribution(:)
      real(c_double), intent(out), allocatable :: sampled_speeds(:,:)
    end subroutine
    
    pure module function do_concurrent_my_velocities(speeds, directions, sampled_speeds) result(my_velocities)
      implicit none
      double precision, intent(in) :: speeds(:,:), directions(:,:,:), sampled_speeds(:,:)
      double precision, allocatable :: my_velocities(:,:,:)
    end function
      
  end interface
  
end module do_concurrent_m
