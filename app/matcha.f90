program matcha_app
  !! Matcha: Motility Analysis of T-Cell Histories in Activation
  use matcha_m, only : matcha, input_t
  use data_partition_m, only: data_partition_t
  use input_m, only : input_t
  implicit none
  type(input_t) :: input
  type(data_partition_t) :: data_partition
  integer a, b

  call data_partition%define_partitions(cardinality=input%ncells)
  a = input%num_cells_
  b = data_partition%last(this_image())
  a = b
  print*, a


  !call data_partition%define_partitions(cardinality=input%num_cells())
  associate(history => matcha(input_t()))
  end associate

  print *
  print *,"----> Matcha done. <----"

end program
