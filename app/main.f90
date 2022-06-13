program tcell
  use tcelsim, only : initialize_positions, create_distribution, move_tcells
  implicit none

  integer ncells, npositions, nintervals
  parameter(ncells = 100, npositions = 25)
  parameter(nintervals = 10)

  double precision vel(nintervals)
  double precision cumulative_distribution(0:nintervals)

  double precision x(ncells,npositions)
  double precision y(ncells,npositions)
  double precision z(ncells,npositions)

  call initialize_positions(x,y,z,ncells,npositions)

  call create_distribution(vel,cumulative_distribution,nintervals)

  call move_tcells(x,y,z,vel,cumulative_distribution,ncells,npositions,nintervals)

end program
