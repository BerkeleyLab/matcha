! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.tx
submodule(distribution_m) distribution_s
  use intrinsic_array_m, only : intrinsic_array_t
  use do_concurrent_m, only : do_concurrent_sampled_speeds, do_concurrent_my_velocities
  
#ifdef USE_CAFFEINE
   use caffeine_assert_m, only : assert
#else
   use assert_m, only : assert
#endif
  
  implicit none

contains
  
  pure function monotonically_increasing(f) result(monotonic)
    double precision, intent(in) :: f(:)
    logical monotonic
    integer i
    monotonic = all([(f(i+1) >= f(i), i=1, size(f)-1)])
  end function

  module procedure construct
    integer i

    call assert(all(sample_distribution(:,2)>=0.D0), "distribution_t%construct: sample_distribution>=0.", &
      intrinsic_array_t(sample_distribution))

    associate(nintervals => size(sample_distribution,1))      
      distribution%vel_ = [(sample_distribution(i,1), i =1, nintervals)]  ! Assign speeds to each distribution bin         
      distribution%cumulative_distribution_ = [0.D0, [(sum(sample_distribution(1:i,2)), i=1, nintervals)]]

      call assert(monotonically_increasing(distribution%cumulative_distribution_), &
        "distribution_t: monotonically_increasing(distribution%cumulative_distribution_)", &
        intrinsic_array_t(distribution%cumulative_distribution_))
    end associate

  end procedure construct

  module procedure cumulative_distribution
    call assert(allocated(self%cumulative_distribution_), &
      "distribution_t%cumulative_distribution: allocated(cumulative_distribution_)")
    my_cumulative_distribution = self%cumulative_distribution_
  end procedure 
  
  module procedure velocities
    
    double precision, allocatable :: sampled_speeds(:,:),  dir(:,:,:)
    integer step
    
    call assert(allocated(self%cumulative_distribution_), &
      "distribution_t%cumulative_distribution: allocated(cumulative_distribution_)")
    call assert(allocated(self%vel_), "distribution_t%cumulative_distribution: allocated(vel_)")

     ! Sample from the distribution
     sampled_speeds = do_concurrent_sampled_speeds(speeds, self%vel_, self%cumulative_distribution())
      
     my_velocities = do_concurrent_my_velocities(speeds, directions, sampled_speeds)

  end procedure

end submodule distribution_s
