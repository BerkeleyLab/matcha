submodule(distribution_m) distribution_s
  use assert_m, only : assert
  use intrinsic_array_m, only : intrinsic_array_t
  implicit none

contains

  module procedure construct
    integer i

    call assert(all(sample_distribution>=0.D0), "distribution_t%construct: sample_distribution>=0.", &
      intrinsic_array_t(sample_distribution))

    associate(nintervals => size(sample_distribution,1))      
      distribution%vel_ = [(dble(i), i =1, nintervals)]  ! Assign speeds to each distribution bin         
      distribution%cumulative_distribution_ = [0.D0, [(sum(sample_distribution(1:i)), i=1, nintervals)]]

      call assert(all([(distribution%cumulative_distribution_(i+1) >= distribution%cumulative_distribution_(i), i=1,nintervals)]),&
        "distribution_t: cumulative_distribution increases monotonically", intrinsic_array_t(sample_distribution))
    end associate

  end procedure construct

  module procedure vel
    call assert(allocated(self%vel_), "distribution_t%vel_: allocated(self%vel_)")
    my_vel = self%vel_
  end procedure

  module procedure cumulative_distribution
    my_cumulative_distribution = self%cumulative_distribution_
  end procedure 

end submodule distribution_s
