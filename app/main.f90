! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program main
  !! Matcha: Motility Analysis of T-Cell Histories in Activation
  use matcha_m, only : matcha, input_t, output_t
  implicit none
  type(output_t) output

  associate(input => input_t())
    output = output_t(input, matcha(input))
    block
      double precision, allocatable :: simulated_distribution(:,:), frequency_distribution(:)
      integer, parameter :: freq=2
      integer num_cells

      num_cells = output%my_num_cells()
      simulated_distribution = output%simulated_distribution()
      frequency_distribution =  num_cells*simulated_distribution(:,freq) ! copy to work around nagfor bug
      call co_sum(frequency_distribution, result_image=1)
      call co_sum(num_cells, result_image=1)
      if (this_image()==1) simulated_distribution(:,freq) = simulated_distribution(:,freq)/dble(num_cells)
    end block
  end associate

  print *
  print *,"----> Matcha done. <----"

end program

