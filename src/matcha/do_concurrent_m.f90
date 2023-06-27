module do_concurrent_m
  use iso_c_binding, only : c_double, c_int
  use t_cell_collection_m, only : t_cell_collection_t, t_cell_collection_bind_C_t
  implicit none
  private
  public :: do_concurrent_sample, do_concurrent_sampled_speeds, do_concurrent_my_velocities, do_concurrent_k
  public :: do_concurrent_angles, do_concurrent_speeds
  public :: do_concurrent_output_distribution
  
  interface

    pure module subroutine do_concurrent_sample(rvalues, val, cumulative_distribution, sampled_values) bind(C)
      implicit none
      real(c_double), intent(in) :: rvalues(:,:), val(:), cumulative_distribution(:)
      real(c_double), intent(out), allocatable :: sampled_values(:,:)
    end subroutine
     
    
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
    
    pure module subroutine do_concurrent_k(speeds, vel, k) bind(C)
      implicit none
      real(c_double), intent(in) :: speeds(:), vel(:)
      integer(c_int), intent(out), allocatable :: k(:)
    end subroutine
    
    pure module subroutine &
      do_concurrent_output_distribution(nintervals, speed, freq, emp_distribution, k, output_distribution) bind(C)
      implicit none
      integer(c_int), intent(in) :: nintervals, speed, freq, k(:)
      real(c_double), intent(in) :: emp_distribution(:,:)
      real(c_double), intent(out), allocatable :: output_distribution(:,:)
    end subroutine
    
    module subroutine do_concurrent_speeds(history, speeds) bind(C)
      implicit none
      type(t_cell_collection_bind_C_t), intent(in) :: history(:)
      real(c_double), intent(out), allocatable :: speeds(:)
    end subroutine

    module subroutine do_concurrent_angles(history, angles) bind(C)
      implicit none
      type(t_cell_collection_bind_C_t), intent(in) :: history(:)
      real(c_double), intent(out), allocatable :: angles(:)
    end subroutine

    
  end interface
  
end module do_concurrent_m
