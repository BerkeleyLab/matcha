module matcha_m
  use t_cell_collection_m, only : t_cell_collection_t
  use distribution_m, only : distribution_t
  use input_m, only : input_t
  implicit none

  interface matcha

    module function matcha(input) result(history)
      implicit none
      type(input_t), intent(in) :: input
      type(t_cell_collection_t), allocatable :: history(:)
    end function

  end interface

contains

  module procedure matcha

    associate( &
      ncells => input%num_cells(), &
      npositions => input%num_positions(), &
      ndim => input%num_dimensions(), &
      nintervals => input%num_intervals(), &
      dt => input%time_step() &
    )

      block
        double precision, parameter :: scale = 100.D0
        double precision, allocatable :: sample_distribution(:), random_positions(:,:), random_4vectors(:,:,:)
        type(distribution_t) distribution
        integer, parameter :: nveldim = 4
        integer step

        allocate(random_positions(ncells,ndim))
        call random_number(random_positions)
        allocate(sample_distribution(nintervals))
        call random_number(sample_distribution)

        associate(nsteps => npositions -1)
          allocate(random_4vectors(ncells,nsteps,nveldim))
          call random_number(random_4vectors)
          sample_distribution = sample_distribution/sum(sample_distribution)
          distribution = distribution_t(sample_distribution)

          associate(random_speeds => random_4vectors(:,:,1), random_directions => random_4vectors(:,:,2:4))
            associate(v => distribution%velocities(random_speeds, random_directions))
              history = [t_cell_collection_t(scale*random_positions, time=0.D0)]
              do step = 1, nsteps
                associate(x => history(step)%positions(), t => history(step)%time())
                  history = [history, t_cell_collection_t(x + v(:,step,:)*dt, t + dt)]
                end associate
              end do
            end associate
          end associate
        end associate
      end block
    end associate

  end procedure

end module
