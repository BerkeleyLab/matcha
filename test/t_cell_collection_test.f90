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
      it("distributes cells across images", check_cell_distribution), &
      it("creates a simulated distribution similar to the empirical distribution",compare_distributions) &
      ] &
    )
  end function

  function compare_distributions() result(result_)
    type(result_t) result_
    integer i,nintervals
    double precision diffmax
    double precision, allocatable :: freq_emp(:),freq_sim(:)
    open(unit=9,file="empf")
    open(unit=10,file="simf")

    read(9,*) nintervals
!    nintervals = 4
    allocate(freq_emp(nintervals),freq_sim(nintervals))
    read(9,*) (freq_emp(i),i=1,nintervals)
    read(10,*) (freq_sim(i),i=1,nintervals)
    diffmax = -1.d0
    do i = 1,nintervals
       diffmax = max(abs(freq_emp(i)-freq_sim(i)),diffmax)
    end do
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
