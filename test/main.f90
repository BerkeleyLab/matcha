  use t_cell_collection_test_m, only : t_cell_collection_test_t
  implicit none
  type(t_cell_collection_test_t) t_cell_collection_test
  integer :: passes=0, tests=0
  call t_cell_collection_test%report(passes, tests)
end
