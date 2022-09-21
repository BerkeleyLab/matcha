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

submodule(do_concurrent_m) do_concurrent_s
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
  
end submodule do_concurrent_s
