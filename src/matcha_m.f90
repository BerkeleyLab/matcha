! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module matcha_m 
  use distribution_m, only : distribution_t
  use subdomain_m, only : subdomain_t
  
  implicit none

  interface

    module function matcha() result(run_completed)
      implicit none
      logical run_completed 
    end function

  end interface
  
end module
