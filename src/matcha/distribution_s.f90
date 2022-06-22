submodule(distribution_m) distribution_s
  implicit none

contains

  module procedure construct
    integer i
    
    associate(nintervals => size(sample_distribution,1))      
      distribution%vel_ = [(dble(i), i =1, nintervals)]  ! Assign speeds to each distribution bin         
      distribution%cumulative_distribution_ = [0.D0, [(sum(sample_distribution(1:i)), i=1, nintervals)]]
    end associate
  end procedure construct
  
  module procedure vel
    my_vel = self%vel_
  end procedure
  
  module procedure cumulative_distribution
    my_cumulative_distribution = self%cumulative_distribution_
  end procedure 
   
end submodule distribution_s
