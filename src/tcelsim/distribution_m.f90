module distribution_m
  implicit none
    private
    public :: create_distribution
contains

  subroutine create_distribution(vel,cumulative_distribution,nintervals)
  
    implicit none
    integer nintervals
    double precision cumulative_distribution(0:nintervals)              
    double precision vel(nintervals)

    !   Local variables      
    integer i
    double precision sum,rr1
    double precision, allocatable :: sample_distribution(:)

    allocate(sample_distribution(nintervals))
      
    !     Create a distribution      
    sum = 0.d0
    do i = 1,nintervals
       call random_number(rr1)
       sample_distribution(i) = rr1
       sum = sum + sample_distribution(i)
    end do

    do i = 1,nintervals
      sample_distribution(i) = sample_distribution(i)/sum
      !        Assign speeds to each distribution bin         
      vel(i) = dble(i)
    end do

    !     Form the cumulative distribution      
    cumulative_distribution(0) = 0.d0
    do i = 1,nintervals
       cumulative_distribution(i) = cumulative_distribution(i-1) + &
                                    sample_distribution(i)
    end do

  end subroutine create_distribution
  
end module distribution_m
