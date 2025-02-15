module t_cell_collection_test_m
  use t_cell_collection_m, only : t_cell_collection_t
  use input_m, only : input_t
  use matcha_m, only : matcha
  implicit none

contains

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

  use t_cell_collection_test_m, only : check_cell_distribution
  implicit none
  logical test_passes
  test_passes = check_cell_distribution()
end
