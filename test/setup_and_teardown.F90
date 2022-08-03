module setup_and_teardown
#ifdef USE_CAFFEINE
    use caffeine_m, only : caf_caffeinate, caf_decaffeinate, caf_error_stop
#endif
    implicit none
contains
    subroutine setup
#ifdef USE_CAFFEINE
        associate(stop_code => caf_caffeinate())
          if (stop_code /= 0) call caf_error_stop(stop_code)
        end associate
#endif
    end subroutine

    subroutine teardown
#ifdef USE_CAFFEINE
        call caf_decaffeinate(0)
#endif
    end subroutine
end module