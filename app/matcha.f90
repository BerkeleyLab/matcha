! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program matcha_app
  !! Matcha: Motility Analysis of T-Cell Histories in Activation
  use matcha_m, only : matcha, input_t
  implicit none

  associate(history => matcha(input_t()))
  end associate

  print *
  print *,"----> Matcha done. <----"

end program
