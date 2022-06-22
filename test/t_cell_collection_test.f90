module t_cell_collection_test
   !! summary: unit tests for T-cell collections
   use garden, only: &
     result_t, test_item_t, describe, it, assert_that
   use t_cell_collection_m, only : t_cell_collection_t
   implicit none

   private
   public :: test_t_cell_collection

contains

  function test_t_cell_collection() result(tests)
    type(test_item_t) tests

    tests = describe( &
     "a t_cell_collection", &
     [it( &
       "is constructed with positions in the specified domain", check_constructed_domain)])
  end function

  function check_constructed_domain() result(result_)
    type(result_t) result_
    integer, parameter :: ncells = 100, ndim = 3
    double precision random_positions(ncells,ndim)
    type(t_cell_collection_t) t_cell_collection
    double precision, parameter :: scale_factor=100.D0
    
    call random_number(random_positions)
    t_cell_collection = t_cell_collection_t(positions=random_positions, scale=scale_factor, time=0.D0)
      result_ = assert_that(all(0.D0 <= t_cell_collection%positions() .and. t_cell_collection%positions() <= scale_factor))
  end function

end module t_cell_collection_test
