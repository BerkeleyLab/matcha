  use iso_c_binding, only : c_double, c_int
  implicit none

  type distribution_t
    double precision, allocatable, dimension(:) :: vel_, cumulative_distribution_
  end type  

  integer, parameter  :: ncells = 6000, npositions = 6000, nveldim = 4, nsteps = npositions - 1
  double precision random_4vectors(ncells,nsteps,nveldim)

  call random_init(repeatable=.true., image_distinct=.true.)
  call random_number(random_4vectors)  
  associate(v => velocities(construct(distribution()), random_4vectors(:,:,1), random_4vectors(:,:,2:4)))
  end associate

contains

  function distribution()
     double precision, allocatable :: distribution(:,:)
     double precision, parameter :: two_pi = 2D0*acos(-1.d0), speed_lower = 0.d0, speed_upper = 6.d0
     integer, parameter :: nintervals = 4
     integer i
     allocate(distribution(nintervals,2))
     associate(range => speed_upper - speed_lower)
       associate(dspeed => range/dble(nintervals))
        do i = 1, size(distribution,1)
          associate(speed_lower_bin => speed_lower + dble(i-1)*dspeed, speed_upper_bin => speed_lower + dble(i)*dspeed)
            associate(speeds => 0.5D0*(speed_lower_bin + speed_upper_bin))
              distribution(i,1) = speeds
              distribution(i,2) = exp(-(speeds-3.d0)**2/2.d0)/dsqrt(two_pi) ! Use normal distribution
            end associate
          end associate
        end do
       end associate
     end associate
     distribution(:,2) = distribution(:,2)/sum(distribution(:,2))
  end function

  pure function construct(sample_distribution) result(distribution)
    double precision, intent(in) :: sample_distribution(:,:)
    type(distribution_t) distribution
    integer i
    associate(nintervals => size(sample_distribution,1))      
      distribution%vel_ = [(sample_distribution(i,1), i =1, nintervals)]  ! Assign speeds to each distribution bin         
      distribution%cumulative_distribution_ = [0.D0, [(sum(sample_distribution(1:i,2)), i=1, nintervals)]]
    end associate
  end function

  pure subroutine do_concurrent_sampled_speeds(speeds, vel, cumulative_distribution, sampled_speeds) bind(C)
    real(c_double), intent(in) :: speeds(:,:), vel(:), cumulative_distribution(:)
    real(c_double), intent(out), allocatable :: sampled_speeds(:,:)
    integer cell, step
    associate(ncells => size(speeds,1), nsteps => size(speeds,2))
      allocate(sampled_speeds(ncells,nsteps))
      do concurrent(cell = 1:ncells, step = 1:nsteps)
        associate(k => findloc(speeds(cell,step) >= cumulative_distribution, value=.false., dim=1)-1)
          sampled_speeds(cell,step) = vel(k)
        end associate
      end do
    end associate
  end subroutine
  
  pure function velocities(self, speeds, directions) result(my_velocities)
    class(distribution_t), intent(in) :: self
    double precision, intent(in) :: speeds(:,:), directions(:,:,:)
    double precision, allocatable :: my_velocities(:,:,:), sampled_speeds(:,:),  dir(:,:,:)
    integer step
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
      allocate(my_velocities, mold=dir)
      do concurrent(step=1:nsteps)
        my_velocities(:,step,1) = sampled_speeds(:,step)*dir(:,step,1)
        my_velocities(:,step,2) = sampled_speeds(:,step)*dir(:,step,2)
        my_velocities(:,step,3) = sampled_speeds(:,step)*dir(:,step,3)
      end do
    end associate
  end function

end
