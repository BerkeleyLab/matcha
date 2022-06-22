submodule(distribution_m) distribution_s
  implicit none


contains

  module procedure create_distribution

    !   Local variables      
    integer i
    double precision, allocatable :: sample_distribution(:)
    
    !     Create a distribution 
    associate(nintervals => size(cumulative_distribution,1)-1)
      allocate(sample_distribution(nintervals))
      call random_number(sample_distribution)
      sample_distribution = sample_distribution/ sum(sample_distribution)
      vel = [(dble(i), i =1, nintervals)]  ! Assign speeds to each distribution bin         
      cumulative_distribution = [(0.D0, sum(sample_distribution(1:i)), i=1,nintervals)] ! Form the cumulative distribution
    end associate

  end procedure create_distribution
  
end submodule distribution_s
