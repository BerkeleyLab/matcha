module t_cell_collection_test_m
  use sourcery_m, only : test_result_t
  use t_cell_collection_m, only : t_cell_collection_t
  use input_m, only : input_t
  use matcha_m, only : matcha
  implicit none

contains

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)

    character(len=*), parameter :: longest_description = "is constructed with positions in the specified domain" 

    test_results = test_result_t( &
      [ character(len=len(longest_description)) :: &
        "is constructed with positions in the specified domain", &
        "distributes cells across images" &
      ], &
      [  check_constructed_domain(), &
         check_cell_distribution() &
       ] &
    )
  end function

  function check_constructed_domain() result(test_passes)
    logical test_passes
    integer, parameter :: ncells = 100, ndim = 3 
    double precision random_positions(ncells,ndim)
    double precision, parameter :: scale_factor=100.D0
    type(t_cell_collection_t) t_cell_collection

    call random_number(random_positions)    
    t_cell_collection = t_cell_collection_t(scale_factor*random_positions,time=0.D0)
         
    associate(constructed_positions => t_cell_collection%positions())
      test_passes = all(0.D0 <=  constructed_positions .and. constructed_positions <= scale_factor)
    end associate
  end function
  
  function check_cell_distribution() result(test_passes)
    logical test_passes
    integer cell_collection_size 
    type(t_cell_collection_t), allocatable :: history(:)

    associate(input => input_t())
      history = matcha(input)
      cell_collection_size = size(history(1)%positions(), 1)
      call co_sum(cell_collection_size)
      test_passes = input%num_cells() .eq. cell_collection_size
    end associate
  end function

end module t_cell_collection_test_m

  use sourcery_m, only : test_result_t
  use t_cell_collection_test_m, only : results
  implicit none
  type(test_result_t), allocatable :: test_results(:)
  test_results = results()
end
