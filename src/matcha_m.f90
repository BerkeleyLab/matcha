module matcha_m 
  use distribution_m, only : distribution_t
  implicit none

  integer :: ncells = 6000, npositions = 6000, ndim = 3
  integer, parameter :: nveldim = 4
  double precision :: dt = 0.1D0

contains

  function matcha() result(run_completed)
    logical run_completed 
    double precision, allocatable :: random_positions(:,:), random_4vectors(:,:,:)
    type(distribution_t) distribution

    associate(empirical_distribution => sample_distribution())
      call random_init(repeatable=.true., image_distinct=.true.)
      allocate(random_positions(ncells,ndim))
      call random_number(random_positions)  
      associate(nsteps => npositions -1)
        allocate(random_4vectors(ncells,nsteps,nveldim))
        call random_number(random_4vectors)  
        distribution = distribution_t(empirical_distribution)
        associate(random_speeds => random_4vectors(:,:,1), random_directions => random_4vectors(:,:,2:4))
          associate(v => distribution%velocities(random_speeds, random_directions))
            run_completed = .true.
          end associate
        end associate
      end associate
    end associate

  end function

  function sample_distribution() result(empirical_distribution)
     double precision, allocatable :: empirical_distribution(:,:)
     integer, parameter :: nintervals = 4
     integer i
     double precision speed_lower,speed_upper
     double precision range,pi,dspeed,sumy
     double precision speed_lower_bin
     double precision speed_upper_bin
     double precision speed_middle_bin
     double precision, allocatable :: speeds(:),probability(:)

     speed_lower = 0.d0
     speed_upper = 6.d0
     range = speed_upper - speed_lower
     pi = acos(-1.d0)
     dspeed = range/dble(nintervals)
     allocate(speeds(nintervals),probability(nintervals))
!    Create normal distribution     
     sumy = 0.d0
     do i = 1,nintervals
        speed_lower_bin = speed_lower + dble(i-1)*dspeed
        speed_upper_bin = speed_lower + dble(i)*dspeed
        speed_middle_bin = 0.5d0*(speed_lower_bin + speed_upper_bin)
        speeds(i) = speed_middle_bin
        probability(i) = exp(-(speeds(i)-3.d0)**2/2.d0)/dsqrt(2.d0*pi) ! Use normal distribution
        sumy = sumy + probability(i)
     end do

     do i = 1,nintervals
        probability(i) = probability(i)/sumy
     end do
      
     allocate(empirical_distribution(nintervals,2))
      
     do i = 1,nintervals
         empirical_distribution(i,1) = speeds(i)
         empirical_distribution(i,2) = probability(i)
     end do
      
  end function
 
end module
