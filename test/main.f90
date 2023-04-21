! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program main
  use subdomain_test_m, only : subdomain_test_t
  implicit none

  type(subdomain_test_t) subdomain_test

  integer :: passes=0, tests=0

  call subdomain_test%report(passes, tests)

  print *
  print '(*(a,:,g0))',"_________ In total, ",passes," of ",tests, " tests pass. _________"
end program
