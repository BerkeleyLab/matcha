! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program main
  !! Matcha: Motility Analysis of T-Cell Histories in Activation
  use matcha_m, only : matcha, input_t, output_t
  implicit none
  type(output_t) output

  associate(input => input_t())
    output = output_t(input, matcha(input))
  end associate

  print *
  print *,"----> Matcha done. <----"

end program
