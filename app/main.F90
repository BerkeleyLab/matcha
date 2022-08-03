! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program main
  !! Matcha: Motility Analysis of T-Cell Histories in Activation
  use matcha_m, only : matcha, input_t, output_t
#ifdef USE_CAFFEINE
  use caffeine_m, only : co_sum, this_image => caf_this_image
#endif
  implicit none
  type(output_t) output

  associate(input => input_t())
    output = output_t(input, matcha(input))
    block
      double precision, allocatable :: simulated_distribution(:,:)
      integer, parameter :: freq=2
      integer num_cells

      num_cells = output%my_num_cells()
      simulated_distribution = output%simulated_distribution()
      simulated_distribution(:,freq) = num_cells*simulated_distribution(:,freq)
      call co_sum(simulated_distribution(:,freq), result_image=1)
      call co_sum(num_cells, result_image=1)
      if (this_image()==1) simulated_distribution(:,freq) = simulated_distribution(:,freq)/dble(num_cells)
    end block
  end associate

  print *
  print *,"----> Matcha done. <----"

end program
