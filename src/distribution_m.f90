module distribution_m
  use do_concurrent_m, only : do_concurrent_sampled_speeds, do_concurrent_my_velocities
  implicit none
  
  type distribution_t
    double precision, allocatable, dimension(:) :: vel_, cumulative_distribution_
  end type  

  interface distribution_t
  
   pure module function construct(sample_distribution) result(distribution)
      implicit none
      double precision, intent(in) :: sample_distribution(:,:)
      type(distribution_t) distribution
    end function
    
  end interface

contains
  
  module procedure construct
    integer i
    if (.not. all(sample_distribution(:,2)>=0.D0)) error stop "negative sample_distribution value(s)"
    associate(nintervals => size(sample_distribution,1))      
      distribution%vel_ = [(sample_distribution(i,1), i =1, nintervals)]  ! Assign speeds to each distribution bin         
      distribution%cumulative_distribution_ = [0.D0, [(sum(sample_distribution(1:i,2)), i=1, nintervals)]]
      associate(f => distribution%cumulative_distribution_)
        if (.not. all([(f(i+1) >= f(i), i=1, size(f)-1)])) error stop "non-monotonic cum dist"
      end associate
    end associate
  end procedure construct

  pure function cumulative_distribution(self) result(my_cumulative_distribution)
    class(distribution_t), intent(in) :: self
    double precision, allocatable :: my_cumulative_distribution(:)
    if (.not. allocated(self%cumulative_distribution_)) error stop "unallocatd cum dist"
    my_cumulative_distribution = self%cumulative_distribution_
  end function
  
  pure function velocities(self, speeds, directions) result(my_velocities)
    class(distribution_t), intent(in) :: self
    double precision, intent(in) :: speeds(:,:), directions(:,:,:)
    double precision, allocatable :: my_velocities(:,:,:)

    double precision, allocatable :: sampled_speeds(:,:),  dir(:,:,:)
    
    if (.not. allocated(self%cumulative_distribution_)) error stop "unallocatd cum dist"
    if (.not. allocated(self%vel_)) error stop "unallocated vel_"
    call do_concurrent_sampled_speeds(speeds, self%vel_, self%cumulative_distribution_, sampled_speeds)
    associate(nsteps => size(speeds,2))
      dir = directions(:,1:nsteps,:)
      associate(dir_mag => sqrt(dir(:,:,1)**2 +dir(:,:,2)**2 + dir(:,:,3)**2))
        associate(dir_mag_ => merge(dir_mag, epsilon(dir_mag), dir_mag/=0.))
          dir(:,:,1) = dir(:,:,1)/dir_mag_
          dir(:,:,2) = dir(:,:,2)/dir_mag_
          dir(:,:,3) = dir(:,:,3)/dir_mag_
        end associate
      end associate
      call do_concurrent_my_velocities(nsteps, dir, sampled_speeds, my_velocities)
    end associate
  end function

end module distribution_m
