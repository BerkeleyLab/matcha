! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program main
  !! Matcha: Motility Analysis of T-Cell Histories in Activation
  use matcha_m, only : matcha, input_t, output_t
#ifdef USE_CAFFEINE
   use caffeine_m, only : caf_caffeinate, caf_decaffeinate
#endif
  implicit none
  type(output_t) output

#ifdef USE_CAFFEINE
  if (caf_caffeinate() /= 0) stop
#endif

  associate(input => input_t())
    output = output_t(input, matcha(input))
  end associate

  print *
  print *,"----> Matcha done. <----"

#ifdef USE_CAFFEINE
  call caf_decaffeinate(0)
#endif
end program
