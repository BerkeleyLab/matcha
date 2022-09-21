module do_concurrent_m
  implicit none
  private
  public :: do_concurrent_sampled_speeds
  
  interface
    
    pure module function do_concurrent_sampled_speeds(speeds, vel, cumulative_distribution) result(sampled_speeds)
      implicit none
      double precision, intent(in) :: speeds(:,:), vel(:), cumulative_distribution(:)
      double precision, allocatable :: sampled_speeds(:,:)
    end function

  end interface
  
end module do_concurrent_m
