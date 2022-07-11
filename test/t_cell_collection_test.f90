module t_cell_collection_test
   !! summary: unit tests for T-cell collections
   use garden, only: &
     result_t, test_item_t, describe, it, assert_that, assert_equals
   use t_cell_collection_m, only : t_cell_collection_t
   use data_partition_m, only : data_partition_t
   use input_m, only : input_t
   use matcha_m, only : matcha
   implicit none

   private
   public :: test_t_cell_collection

contains

  function test_t_cell_collection() result(tests)
    type(test_item_t) tests

    tests = describe( &
     "a t_cell_collection", &
     [it( "is constructed with positions in the specified domain", check_constructed_domain), &
      it("distributes cells across images", check_cell_distribution) &
      ] &
    )
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

  function delete_me() result(result_)
    type(result_t) result_
    integer, parameter :: ncells = 50, ndim = 3
    double precision, allocatable :: positions(:,:)
    type(t_cell_collection_t) t_cell_collection
    type(data_partition_t) data_partition
    integer cell_collection_size

    call data_partition%define_partitions(cardinality=ncells)

    associate(me => this_image())
      associate(my_num_cells => data_partition%last(me) - data_partition%first(me) + 1)
        allocate(positions(my_num_cells, ndim), source = 0.D0)
        t_cell_collection = t_cell_collection_t(positions, time=0.D0)
        cell_collection_size = size(t_cell_collection%positions(), 1)
        call co_sum(cell_collection_size)
        result_ = assert_equals(ncells, cell_collection_size)
      end associate
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
      result_ = assert_equals(input%ncells(), cell_collection_size)
    end associate
  end function

end module t_cell_collection_test
