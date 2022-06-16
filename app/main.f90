program tcell
  use tcelsim, only : initialize_positions, create_distribution, create_rand_num_table, move_tcells
  implicit none

  integer ncells, npositions, nintervals, nveldim
  parameter(ncells = 100, npositions = 25, nveldim = 4)
  parameter(nintervals = 10)

  double precision vel(nintervals)
  double precision cumulative_distribution(0:nintervals)

  double precision, allocatable :: random_number_table(:,:,:)
  double precision, allocatable :: x(:,:)
  double precision, allocatable :: y(:,:)
  double precision, allocatable :: z(:,:)
  
  allocate(random_number_table(ncells,npositions,nveldim))
  allocate(x(ncells,npositions))
  allocate(y(ncells,npositions))
  allocate(z(ncells,npositions))

  call initialize_positions(x,y,z,ncells,npositions)

  call create_distribution(vel,cumulative_distribution,nintervals)
  
  call create_rand_num_table(ncells,npositions,random_number_table,nveldim)

  call move_tcells(x,y,z,vel,cumulative_distribution,random_number_table,ncells,npositions,nintervals,nveldim)

end program
