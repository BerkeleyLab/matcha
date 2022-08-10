! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(output_m) output_s
  implicit none
  
contains

  pure module function construct(input, history) result(output)
    !! Construct a new output_t object
    type(input_t), intent(in) :: input
    type(t_cell_collection_t), intent(in) :: history(:)
    type(output_t) :: output
    output%input_ = input
    output%history_ = history
  end function
  
  pure module function my_num_cells(self) result(num_cells)
    class(output_t), intent(in) :: self
    integer num_cells
    num_cells = size(self%history_(1)%positions(), 1)
  end function

  pure module function simulated_distribution(self) result(output_distribution)
    !! The result is a histogram calculated from the simulation output
    class(output_t), intent(in) :: self
    double precision, allocatable :: output_distribution(:,:)
    integer i
    integer, allocatable :: k(:)
    double precision, allocatable :: vel(:)
    
    integer, parameter :: speed=1, freq=2 ! subscripts for speeds and frequencies

    associate(speeds => sim_speeds(self%history_))
      associate(emp_distribution => self%input_%sample_distribution())
        associate(nintervals => size(emp_distribution(:,1)))
          allocate(output_distribution(nintervals,2))
          output_distribution(:,freq) = 0.d0
          output_distribution(:,speed) = emp_distribution(:,speed)
          associate(dvel_half => (emp_distribution(2,speed)-emp_distribution(1,speed))/2.d0)
            vel = [emp_distribution(1,speed) - dvel_half, [(emp_distribution(i,speed) + dvel_half, i=1,nintervals)]]
            associate(nspeeds => size(speeds))
              allocate(k(nspeeds))
              do concurrent(i = 1:nspeeds)
                k(i) = findloc(speeds(i) >= vel, value=.false., dim=1)-1
              end do
            end associate
            do concurrent(i = 1:size(output_distribution,1))
              output_distribution(i,freq) = count(k==i)
            end do
            output_distribution(:,freq) = output_distribution(:,freq)/sum(output_distribution(:,freq))
          end associate
        end associate
      end associate
    end associate

  contains

    pure function sim_speeds(history) result(speeds)
      type(t_cell_collection_t), intent(in) :: history(:)
      double precision, allocatable :: speeds(:)

      integer, parameter :: nspacedims=3
      integer i, j, k
      double precision, allocatable :: x(:,:,:)
  
      associate( &
        npositions => size(history,1), &
        ncells => size(history(1)%positions(),1) &
      )   
        allocate(x(npositions,ncells,nspacedims))
        do concurrent(i=1:npositions)
          x(i,:,:) = history(i)%positions()
        end do
        associate(t => history%time())
          allocate(speeds(ncells*(npositions-1)))
          do concurrent(i = 1:npositions-1, j = 1:ncells)
            associate( &
              u => (x(i+1,j,:) - x(i,j,:))/(t(i+1) - t(i)), &
              ij => i + (j-1)*(npositions-1) &
            )   
              speeds(ij) = sqrt(sum([(u(k)**2, k=1,nspacedims)]))
            end associate
          end do
        end associate
      end associate
    end function

  end function simulated_distribution

end submodule output_s
