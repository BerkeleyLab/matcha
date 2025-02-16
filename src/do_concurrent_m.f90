module do_concurrent_m
  use iso_c_binding, only : c_double, c_int
  use t_cell_collection_m, only : t_cell_collection_bind_C_t
  implicit none
  private
  public :: do_concurrent_sampled_speeds, do_concurrent_my_velocities
  
  interface
    
    pure module subroutine do_concurrent_sampled_speeds(speeds, vel, cumulative_distribution, sampled_speeds) bind(C)
      implicit none
      real(c_double), intent(in) :: speeds(:,:), vel(:), cumulative_distribution(:)
      real(c_double), intent(out), allocatable :: sampled_speeds(:,:)
    end subroutine
    
    pure module subroutine do_concurrent_my_velocities(nsteps, dir, sampled_speeds, my_velocities) bind(C)
      implicit none
      integer(c_int), intent(in) :: nsteps
      real(c_double), intent(in) :: dir(:,:,:), sampled_speeds(:,:)
      real(c_double), intent(out), allocatable :: my_velocities(:,:,:)
    end subroutine
    
  end interface
  
end module do_concurrent_m
