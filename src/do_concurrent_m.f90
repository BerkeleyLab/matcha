module do_concurrent_m
  use iso_c_binding, only : c_double, c_int
  use t_cell_collection_m, only : t_cell_collection_bind_C_t
  implicit none
  
  interface
    
  end interface
  
contains
  
  pure subroutine do_concurrent_sampled_speeds(speeds, vel, cumulative_distribution, sampled_speeds) bind(C)
    real(c_double), intent(in) :: speeds(:,:), vel(:), cumulative_distribution(:)
    real(c_double), intent(out), allocatable :: sampled_speeds(:,:)
    integer cell, step
    associate(ncells => size(speeds,1), nsteps => size(speeds,2))
      allocate(sampled_speeds(ncells,nsteps))
      do concurrent(cell = 1:ncells, step = 1:nsteps)
        associate(k => findloc(speeds(cell,step) >= cumulative_distribution, value=.false., dim=1)-1)
          sampled_speeds(cell,step) = vel(k)
        end associate
      end do
    end associate
  end subroutine
  
  pure subroutine do_concurrent_my_velocities(nsteps, dir, sampled_speeds, my_velocities) bind(C)
    integer(c_int), intent(in) :: nsteps
    real(c_double), intent(in) :: dir(:,:,:), sampled_speeds(:,:)
    real(c_double), intent(out), allocatable :: my_velocities(:,:,:)
    integer step
    
    if(allocated(my_velocities)) deallocate(my_velocities)
    allocate(my_velocities, mold=dir)
    
    do concurrent(step=1:nsteps)
      my_velocities(:,step,1) = sampled_speeds(:,step)*dir(:,step,1)
      my_velocities(:,step,2) = sampled_speeds(:,step)*dir(:,step,2)
      my_velocities(:,step,3) = sampled_speeds(:,step)*dir(:,step,3)
    end do
    
  end subroutine
  
end module do_concurrent_m
