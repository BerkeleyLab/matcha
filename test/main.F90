! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program main
  use julienne_m, only : command_line_t
  use subdomain_test_m, only : subdomain_test_t
  use mimetic_test_m, only : mimetic_test_t
  use t_cell_collection_test_m, only : t_cell_collection_test_t
  use matcha_test_m, only : matcha_test_t
  implicit none

  type(subdomain_test_t) subdomain_test
  type(mimetic_test_t) mimetic_test
  type(t_cell_collection_test_t) t_cell_collection_test
  type(matcha_test_t) matcha_test

  type(command_line_t) command_line

  integer :: passes=0, tests=0, skips=0

  character(len=*), parameter :: usage = & 
    new_line('') // new_line('') // &
    'Usage: fpm test -- [--help] | [--contains <substring>]' // &
    new_line('') // new_line('') // &
    'where square brackets ([]) denote optional arguments, a pipe (|) separates alternative arguments,' // new_line('') // &
    'angular brackets (<>) denote a user-provided value, and passing a substring limits execution to' // new_line('') // &
    'the tests with test subjects or test descriptions containing the user-specified substring.' // new_line('') 

  if (command_line%argument_present([character(len=len("--help"))::"--help","-h"])) stop usage

  print "(a)", new_line("") // "Append '-- --help' or '-- -h' to your `fpm test` command to display usage information."

  call subdomain_test%report(passes, tests, skips)
  call mimetic_test%report(passes, tests, skips)
  call t_cell_collection_test%report(passes, tests, skips)
  call matcha_test%report(passes, tests, skips)

#if HAVE_MULTI_IMAGE_SUPPORT
  if (this_image()==1) then
#endif
    print *
    print '(*(a,:,g0))', "_________ In total, ",passes," of ",tests, " tests pass.  ", skips, " tests were skipped. _________"
    if (passes + skips /= tests) error stop "Some executed tests failed."
#if HAVE_MULTI_IMAGE_SUPPORT
  end if
#endif

end program
