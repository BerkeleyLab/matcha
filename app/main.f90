program matcha
  use matcha_m, only : initialize_positions, move_tcells
  use distribution_m, only : distribution_t
  implicit none

  integer, parameter :: ncells = 100, npositions = 25, nveldim = 4, ndim = 3, nintervals = 10
  integer, parameter :: nsteps = npositions - 1
  double precision, allocatable, dimension(:,:) :: x, y, z, random_positions

  allocate(x(ncells,npositions))
  allocate(y(ncells,npositions))
  allocate(z(ncells,npositions))
  allocate(random_positions(ncells,ndim))

  call random_number(random_positions)
  call initialize_positions(random_positions, x(:,1),y(:,1),z(:,1))

  block
    double precision, allocatable :: random_velocities(:,:,:), sample_distribution(:)
    type(distribution_t) distribution
    
    allocate(random_velocities(ncells,nsteps,nveldim))
    allocate(sample_distribution(nintervals))

    call random_number(sample_distribution)
    sample_distribution = sample_distribution/sum(sample_distribution)
    
    call random_number(random_velocities)

    distribution = distribution_t(sample_distribution)
    associate(random_speeds => random_velocities(:,:,1), random_directions => random_velocities(:,:,2:4))
      call move_tcells(distribution, random_speeds, random_directions, x,y,z)
    end associate
  end block

  print *
  print *,"----> Matcha done. <----"

end program
