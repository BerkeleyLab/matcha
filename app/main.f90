program tcell
  use tcelsim, only : initialize_positions, create_distribution, move_tcells
  implicit none

  integer ncells, npositions, nintervals, nveldim, ndim
  parameter(ncells = 100, npositions = 25, nveldim = 4, ndim = 3)
  parameter(nintervals = 10)

  double precision vel(nintervals)
  double precision cumulative_distribution(nintervals+1)

  double precision, allocatable :: random_number_table(:,:,:)
  double precision, allocatable :: random_positions(:,:)
  double precision, allocatable :: x(:,:)
  double precision, allocatable :: y(:,:)
  double precision, allocatable :: z(:,:)
  
  allocate(random_number_table(ncells,npositions,nveldim))
  allocate(random_positions(ncells,ndim))
  allocate(x(ncells,npositions))
  allocate(y(ncells,npositions))
  allocate(z(ncells,npositions))

  call random_number(random_positions)

  call initialize_positions(x(:,1),y(:,1),z(:,1),random_positions)

  call create_distribution(vel,cumulative_distribution,nintervals)
  
  call random_number(random_number_table)

  call move_tcells(x,y,z,vel,cumulative_distribution,random_number_table,ncells,npositions,nintervals,nveldim)

end program
