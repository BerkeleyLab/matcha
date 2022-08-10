! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(input_m) input_s
  implicit none

contains
      
  pure module function num_cells(self) result(n)
    class(input_t), intent(in) :: self
    integer n
    n = self%num_cells_
  end function
   
  pure module function num_positions(self) result(n)
    class(input_t), intent(in) :: self
    integer n
    n = self%num_positions_
  end function
  
  pure module function num_dimensions(self) result(n)
    class(input_t), intent(in) :: self
    integer n
    n = self%num_dimensions_
  end function

  pure module function num_intervals(self) result(n)
    class(input_t), intent(in) :: self
    integer n
    n = self%num_intervals_
  end function

  pure module function time_step(self) result(dt)
    class(input_t), intent(in) :: self
    double precision dt
    dt = self%time_step_
  end function

  pure module function sample_distribution(self) result(empirical_distribution)
    class(input_t), intent(in) :: self
    double precision, allocatable :: empirical_distribution(:,:)
     integer i,nintervals
     double precision speed_lower,speed_upper
     double precision range,pi,dspeed,sumy
     double precision speed_lower_bin
     double precision speed_upper_bin
     double precision speed_middle_bin
     double precision, allocatable :: speeds(:),probability(:)
     nintervals = self%num_intervals_

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
 
end submodule input_s
