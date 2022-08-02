module setup_and_teardown
#ifdef USE_CAFFEINE
    use caffeine_m, only : caf_caffeinate, caf_decaffeinate
#endif
    implicit none
contains
    subroutine setup
#ifdef USE_CAFFEINE
        if (caf_caffeinate() /= 0) stop
#endif
    end subroutine

    subroutine teardown
#ifdef USE_CAFFEINE
        call caf_decaffeinate(0)
#endif
    end subroutine
end module