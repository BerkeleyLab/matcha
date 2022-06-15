program tcell
  use tcelsim, only : initialize_positions, create_distribution, create_rand_num_table, move_tcells
  implicit none

  integer ncells, npositions, nintervals
  parameter(ncells = 100, npositions = 25)
  parameter(nintervals = 10)

  double precision vel(nintervals)
  double precision cumulative_distribution(0:nintervals)

  double precision random_number_table(ncells,npositions,4)
  double precision x(ncells,npositions)
  double precision y(ncells,npositions)
  double precision z(ncells,npositions)

  call initialize_positions(x,y,z,ncells,npositions)

  call create_distribution(vel,cumulative_distribution,nintervals)
  
  call create_rand_num_table(ncells,npositions,random_number_table)

  call move_tcells(x,y,z,vel,cumulative_distribution,random_number_table,ncells,npositions,nintervals)

end program
