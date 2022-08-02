! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module t_cell_collection_test
   !! summary: unit tests for T-cell collections
   use garden, only: &
     result_t, test_item_t, describe, it, assert_that, assert_equals, assert_equals_within_absolute
   use t_cell_collection_m, only : t_cell_collection_t
   use input_m, only : input_t
   use output_m, only : output_t
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
    type(result_t) result_
    type(output_t) output
    
    integer, parameter :: speed=1, freq=2 ! subscripts for speeds and frequencies

    associate(input => input_t())
      output = output_t(input, matcha(input))
      associate( &
        empirical_distribution => input%sample_distribution(), &
        simulated_distribution => output%simulated_distribution() &
      )
        associate( &
          diffmax_speeds=> maxval(abs(empirical_distribution(:,speed)-simulated_distribution(:,speed))), &
          diffmax_freqs => maxval(abs(empirical_distribution(:,freq)-simulated_distribution(:,freq))) &
        )
          result_ = &
            assert_equals_within_absolute(0.D0, diffmax_freqs, 1.D-02, "frequencies match empirical distribution") .and. &
            assert_equals_within_absolute(0.D0, diffmax_speeds, 1.D-02, "speeds match empirical distribution")
        end associate
      end associate
    end associate
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
