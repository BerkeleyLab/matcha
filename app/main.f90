program tcell
  use tcelsim, only : initialize_positions, create_distribution, move_tcells
  implicit none

  integer i,j,k
  integer ncells, npositions, nintervals
  parameter(ncells = 100, npositions = 25)
  parameter(nintervals = 10)

  double precision vel(nintervals)
  double precision cumulative_distribution(0:nintervals)

  double precision random_number_table(ncells,npositions,4)
  double precision x(ncells,npositions)
  double precision y(ncells,npositions)
  double precision z(ncells,npositions)
  
  do i = 1,ncells
    do j = 1,npositions
      do k = 1,4
        call random_number(random_number_table(i,j,k))
      end do
    end do
  end do

  call initialize_positions(x,y,z,ncells,npositions)

  call create_distribution(vel,cumulative_distribution,nintervals)

  call move_tcells(x,y,z,vel,cumulative_distribution,random_number_table,ncells,npositions,nintervals)

end program
