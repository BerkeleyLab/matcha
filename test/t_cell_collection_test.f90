! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module t_cell_collection_test
   !! summary: unit tests for T-cell collections
   use garden, only: &
     result_t, test_item_t, describe, it, assert_that, assert_equals, assert_equals_within_absolute
   use t_cell_collection_m, only : t_cell_collection_t
   use input_m, only : input_t
   use matcha_m, only : matcha
   use distribution_m, only: distribution_t
   implicit none

   private
   public :: test_t_cell_collection

contains

  function test_t_cell_collection() result(tests)
    type(test_item_t) tests

    tests = describe( &
     "a t_cell_collection", &
     [it( "is constructed with positions in the specified domain", check_constructed_domain), &
      it("distributes cells across images", check_cell_distribution), &
      it("creates a simulated distribution similar to the empirical distribution",compare_distributions) &
     ] &
    )
  end function

  function compare_distributions() result(result_)
    implicit none
    type(result_t) result_
    type(distribution_t) distribution

    associate(input => input_t())
      associate(empirical_distribution => input%sample_distribution())
        associate(sim_distribution => distribution%build_distribution(empirical_distribution,sim_speeds(matcha(input))))
          associate(diffmax => maxval(abs(empirical_distribution(:,2)-sim_distribution(:,2))))
            result_ = assert_equals_within_absolute(0.D0, diffmax, 1.D-02, "distribution matches empirical distribution")
          end associate
        end associate
      end associate
    end associate

  contains

    pure function sim_speeds(history)
      type(t_cell_collection_t), intent(in) :: history(:)
      double precision, allocatable :: sim_speeds(:)
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
          allocate(sim_speeds(ncells*(npositions-1)))
          do concurrent(j = 1:ncells, i = 1:npositions-1)
            associate( &
              u => (x(i+1,j,:) - x(i,j,:))/(t(i+1) - t(i)), &
              ij => i + (j-1)*(npositions-1) &
            )
              sim_speeds(ij) = sqrt(sum([(u(k)**2, k=1,nspacedims)]))
            end associate
          end do
        end associate
      end associate
    end function

  end function  
  
  function check_constructed_domain() result(result_)
    type(result_t) result_
    integer, parameter :: ncells = 100, ndim = 3
    double precision random_positions(ncells,ndim)
    double precision, parameter :: scale_factor=100.D0
    type(t_cell_collection_t) t_cell_collection

    call random_number(random_positions)    
    t_cell_collection = t_cell_collection_t(scale_factor*random_positions,time=0.D0)
    
    associate(constructed_positions => t_cell_collection%positions())
      result_ = assert_that( &
        all(0.D0 <=  constructed_positions .and. constructed_positions <= scale_factor), "position(s) out of range" &
      )
    end associate
  end function

  function check_cell_distribution() result(result_)
    type(result_t) result_
    integer cell_collection_size 
    type(t_cell_collection_t), allocatable :: history(:)

    associate(input => input_t())
      history = matcha(input)
      cell_collection_size = size(history(1)%positions(), 1)
      call co_sum(cell_collection_size)
      result_ = assert_equals(input%num_cells(), cell_collection_size)
    end associate
  end function

end module t_cell_collection_test
