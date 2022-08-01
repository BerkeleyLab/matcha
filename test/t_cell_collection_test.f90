! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module t_cell_collection_test
   !! summary: unit tests for T-cell collections
   use garden, only: &
     result_t, test_item_t, describe, it, assert_that, assert_equals
   use t_cell_collection_m, only : t_cell_collection_t
   use data_partition_m, only : data_partition_t
   use input_m, only : input_t
   use matcha_m, only : matcha, input_t
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

    integer i,j,k,ij,npositions,ncells
    double precision dx,dy,dz,dt,diffmax
    double precision, allocatable :: x(:,:,:),t(:),sim_speeds(:)
    type(distribution_t) distribution
  
    associate(input => input_t())
      associate(empirical_distribution => input%sample_distribution(),nintervals => input%num_intervals())
        distribution = distribution_t(empirical_distribution)
        associate(history => matcha(input_t()))
          npositions = size(history,1)
          ncells = size(history(1)%positions(),1)
          allocate(x(npositions,ncells,3),t(npositions))
          allocate(sim_speeds(ncells*(npositions-1)))
          do i = 1,npositions
            associate(xcell => history(i)%positions(),times => history(i)%time())
              x(i,:,:) = xcell(:,:)
              t(i) = times
            end associate
          end do
          ij = 0
          do j = 1,ncells
            do i = 1,npositions-1
               ij = ij + 1
               dx = x(i+1,j,1) - x(i,j,1)
               dy = x(i+1,j,2) - x(i,j,2)
               dz = x(i+1,j,3) - x(i,j,3)
               dt = t(i+1) - t(i)
               sim_speeds(ij) = dsqrt((dx/dt)**2 + (dy/dt)**2 + (dz/dt)**2)
            end do
          end do
        end associate
        associate(sim_distribution => distribution%build_distribution(empirical_distribution,sim_speeds))
          diffmax = -1.d0
          do i = 1,nintervals
             diffmax = max(abs(empirical_distribution(i,2)-sim_distribution(i,2)),diffmax)
          end do
        end associate
      end associate
    end associate
!    print*,diffmax
    result_ = assert_that(diffmax < .01, "distributions differ by more than 1 percent")
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
