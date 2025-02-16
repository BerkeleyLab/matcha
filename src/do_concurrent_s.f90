submodule(do_concurrent_m) do_concurrent_s
  use iso_c_binding, only : c_f_pointer
  implicit none

contains
  
  module procedure do_concurrent_sampled_speeds
  
    integer cell, step
    
    associate(ncells => size(speeds,1), nsteps => size(speeds,2))
      allocate(sampled_speeds(ncells,nsteps))
      do concurrent(cell = 1:ncells, step = 1:nsteps)
        associate(k => findloc(speeds(cell,step) >= cumulative_distribution, value=.false., dim=1)-1)
          sampled_speeds(cell,step) = vel(k)
        end associate
      end do
    end associate
    
  end procedure
  
  module procedure do_concurrent_my_velocities
  
    integer step
    
    if(allocated(my_velocities)) deallocate(my_velocities)
    allocate(my_velocities, mold=dir)
    
    do concurrent(step=1:nsteps)
      my_velocities(:,step,1) = sampled_speeds(:,step)*dir(:,step,1)
      my_velocities(:,step,2) = sampled_speeds(:,step)*dir(:,step,2)
      my_velocities(:,step,3) = sampled_speeds(:,step)*dir(:,step,3)
    end do
    
  end procedure
  
end submodule do_concurrent_s
