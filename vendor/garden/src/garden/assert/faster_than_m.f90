module garden_assert_faster_than_m
    use iso_fortran_env, only: int64
    use iso_varying_string, only: varying_string, var_str
    use strff, only: to_string
    use garden_messages_m, only: &
            make_faster_than_failure_message, &
            make_faster_than_success_message, &
            with_user_message
    use garden_result_m, only: result_t, fail, succeed
    use garden_test_interfaces_m, only: computation_i

    implicit none
    private
    public :: assert_faster_than

    interface assert_faster_than
        module procedure assert_faster_than_absolute_bracketed
        module procedure assert_faster_than_absolute_bracketed_with_message_c
        module procedure assert_faster_than_absolute_bracketed_with_message_s
        module procedure assert_faster_than_absolute_bracketed_with_messages_cc
        module procedure assert_faster_than_absolute_bracketed_with_messages_cs
        module procedure assert_faster_than_absolute_bracketed_with_messages_sc
        module procedure assert_faster_than_absolute_bracketed_with_messages_ss
        module procedure assert_faster_than_absolute_simple
        module procedure assert_faster_than_absolute_simple_with_message_c
        module procedure assert_faster_than_absolute_simple_with_message_s
        module procedure assert_faster_than_absolute_simple_with_messages_cc
        module procedure assert_faster_than_absolute_simple_with_messages_cs
        module procedure assert_faster_than_absolute_simple_with_messages_sc
        module procedure assert_faster_than_absolute_simple_with_messages_ss
        module procedure assert_faster_than_relative_bracketed
        module procedure assert_faster_than_relative_bracketed_with_message_c
        module procedure assert_faster_than_relative_bracketed_with_message_s
        module procedure assert_faster_than_relative_bracketed_with_messages_cc
        module procedure assert_faster_than_relative_bracketed_with_messages_cs
        module procedure assert_faster_than_relative_bracketed_with_messages_sc
        module procedure assert_faster_than_relative_bracketed_with_messages_ss
        module procedure assert_faster_than_relative_simple
        module procedure assert_faster_than_relative_simple_with_message_c
        module procedure assert_faster_than_relative_simple_with_message_s
        module procedure assert_faster_than_relative_simple_with_messages_cc
        module procedure assert_faster_than_relative_simple_with_messages_cs
        module procedure assert_faster_than_relative_simple_with_messages_sc
        module procedure assert_faster_than_relative_simple_with_messages_ss
    end interface
contains
    function assert_faster_than_absolute_bracketed( &
            reference, &
            before, &
            computation, &
            after, &
            iterations) &
            result(result__)
        double precision, intent(in) :: reference
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                before, &
                computation, &
                after, &
                iterations, &
                var_str(""), &
                var_str(""))
    end function

    function assert_faster_than_absolute_bracketed_with_message_c( &
            reference, &
            before, &
            computation, &
            after, &
            iterations, &
            message) &
            result(result__)
        double precision, intent(in) :: reference
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                before, &
                computation, &
                after, &
                iterations, &
                var_str(message), &
                var_str(message))
    end function

    function assert_faster_than_absolute_bracketed_with_message_s( &
            reference, &
            before, &
            computation, &
            after, &
            iterations, &
            message) &
            result(result__)
        double precision, intent(in) :: reference
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                before, &
                computation, &
                after, &
                iterations, &
                message, &
                message)
    end function

    function assert_faster_than_absolute_bracketed_with_messages_cc( &
            reference, &
            before, &
            computation, &
            after, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        double precision, intent(in) :: reference
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                before, &
                computation, &
                after, &
                iterations, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    function assert_faster_than_absolute_bracketed_with_messages_cs( &
            reference, &
            before, &
            computation, &
            after, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        double precision, intent(in) :: reference
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                before, &
                computation, &
                after, &
                iterations, &
                var_str(success_message), &
                failure_message)
    end function

    function assert_faster_than_absolute_bracketed_with_messages_sc( &
            reference, &
            before, &
            computation, &
            after, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        double precision, intent(in) :: reference
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                before, &
                computation, &
                after, &
                iterations, &
                success_message, &
                var_str(failure_message))
    end function

    function assert_faster_than_absolute_bracketed_with_messages_ss( &
            reference, &
            before, &
            computation, &
            after, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        double precision, intent(in) :: reference
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        integer :: i
        integer(int64) :: start_time
        integer(int64) :: end_time
        integer(int64) :: clock_rate
        double precision :: total_time
        double precision :: average_time

        total_time = 0.0d0
        do i = 1, iterations
            call before
            call system_clock(start_time, clock_rate)
            call computation
            call system_clock(end_time)
            call after
            total_time = total_time + dble(end_time - start_time)/dble(clock_rate)
        end do
        average_time = total_time / dble(iterations)
        if (average_time < reference) then
            result__ = succeed(with_user_message( &
                    make_faster_than_success_message( &
                            to_string(reference), &
                            to_string(average_time), &
                            to_string(iterations)), &
                    success_message))
        else
            result__ = fail(with_user_message( &
                    make_faster_than_failure_message( &
                            to_string(reference), &
                            to_string(average_time), &
                            to_string(iterations)), &
                    failure_message))
        end if
    end function

    function assert_faster_than_absolute_simple( &
            reference, &
            computation, &
            iterations) &
            result(result__)
        double precision, intent(in) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                computation, &
                iterations, &
                var_str(""), &
                var_str(""))
    end function

    function assert_faster_than_absolute_simple_with_message_c( &
            reference, &
            computation, &
            iterations, &
            message) &
            result(result__)
        double precision, intent(in) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                computation, &
                iterations, &
                var_str(message), &
                var_str(message))
    end function

    function assert_faster_than_absolute_simple_with_message_s( &
            reference, &
            computation, &
            iterations, &
            message) &
            result(result__)
        double precision, intent(in) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                computation, &
                iterations, &
                message, &
                message)
    end function

    function assert_faster_than_absolute_simple_with_messages_cc( &
            reference, &
            computation, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        double precision, intent(in) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                computation, &
                iterations, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    function assert_faster_than_absolute_simple_with_messages_cs( &
            reference, &
            computation, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        double precision, intent(in) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                computation, &
                iterations, &
                var_str(success_message), &
                failure_message)
    end function

    function assert_faster_than_absolute_simple_with_messages_sc( &
            reference, &
            computation, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        double precision, intent(in) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                computation, &
                iterations, &
                success_message, &
                var_str(failure_message))
    end function

    function assert_faster_than_absolute_simple_with_messages_ss( &
            reference, &
            computation, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        double precision, intent(in) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        integer :: i
        integer(int64) :: start_time
        integer(int64) :: end_time
        integer(int64) :: clock_rate
        double precision :: total_time
        double precision :: average_time

        total_time = 0.0d0
        do i = 1, iterations
            call system_clock(start_time, clock_rate)
            call computation
            call system_clock(end_time)
            total_time = total_time + dble(end_time - start_time)/dble(clock_rate)
        end do
        average_time = total_time / dble(iterations)
        if (average_time < reference) then
            result__ = succeed(with_user_message( &
                    make_faster_than_success_message( &
                            to_string(reference), &
                            to_string(average_time), &
                            to_string(iterations)), &
                    success_message))
        else
            result__ = fail(with_user_message( &
                    make_faster_than_failure_message( &
                            to_string(reference), &
                            to_string(average_time), &
                            to_string(iterations)), &
                    failure_message))
        end if
    end function

    function assert_faster_than_relative_bracketed( &
            reference_before, &
            reference, &
            reference_after, &
            before, &
            computation, &
            after, &
            iterations) &
            result(result__)
        procedure(computation_i) :: reference_before
        procedure(computation_i) :: reference
        procedure(computation_i) :: reference_after
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference_before, &
                reference, &
                reference_after, &
                before, &
                computation, &
                after, &
                iterations, &
                var_str(""), &
                var_str(""))
    end function

    function assert_faster_than_relative_bracketed_with_message_c( &
            reference_before, &
            reference, &
            reference_after, &
            before, &
            computation, &
            after, &
            iterations, &
            message) &
            result(result__)
        procedure(computation_i) :: reference_before
        procedure(computation_i) :: reference
        procedure(computation_i) :: reference_after
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference_before, &
                reference, &
                reference_after, &
                before, &
                computation, &
                after, &
                iterations, &
                var_str(message), &
                var_str(message))
    end function

    function assert_faster_than_relative_bracketed_with_message_s( &
            reference_before, &
            reference, &
            reference_after, &
            before, &
            computation, &
            after, &
            iterations, &
            message) &
            result(result__)
        procedure(computation_i) :: reference_before
        procedure(computation_i) :: reference
        procedure(computation_i) :: reference_after
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference_before, &
                reference, &
                reference_after, &
                before, &
                computation, &
                after, &
                iterations, &
                message, &
                message)
    end function

    function assert_faster_than_relative_bracketed_with_messages_cc( &
            reference_before, &
            reference, &
            reference_after, &
            before, &
            computation, &
            after, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        procedure(computation_i) :: reference_before
        procedure(computation_i) :: reference
        procedure(computation_i) :: reference_after
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference_before, &
                reference, &
                reference_after, &
                before, &
                computation, &
                after, &
                iterations, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    function assert_faster_than_relative_bracketed_with_messages_cs( &
            reference_before, &
            reference, &
            reference_after, &
            before, &
            computation, &
            after, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        procedure(computation_i) :: reference_before
        procedure(computation_i) :: reference
        procedure(computation_i) :: reference_after
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference_before, &
                reference, &
                reference_after, &
                before, &
                computation, &
                after, &
                iterations, &
                var_str(success_message), &
                failure_message)
    end function

    function assert_faster_than_relative_bracketed_with_messages_sc( &
            reference_before, &
            reference, &
            reference_after, &
            before, &
            computation, &
            after, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        procedure(computation_i) :: reference_before
        procedure(computation_i) :: reference
        procedure(computation_i) :: reference_after
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference_before, &
                reference, &
                reference_after, &
                before, &
                computation, &
                after, &
                iterations, &
                success_message, &
                var_str(failure_message))
    end function

    function assert_faster_than_relative_bracketed_with_messages_ss( &
            reference_before, &
            reference, &
            reference_after, &
            before, &
            computation, &
            after, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        procedure(computation_i) :: reference_before
        procedure(computation_i) :: reference
        procedure(computation_i) :: reference_after
        procedure(computation_i) :: before
        procedure(computation_i) :: computation
        procedure(computation_i) :: after
        integer, intent(in) :: iterations
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        integer :: i
        integer(int64) :: start_time
        integer(int64) :: end_time
        double precision :: total_time
        double precision :: average_time
        integer(int64) :: reference_start_time
        integer(int64) :: reference_end_time
        integer(int64) :: clock_rate
        double precision :: reference_total_time
        double precision :: reference_average_time

        total_time = 0.0d0
        reference_total_time = 0.0d0
        do i = 1, iterations
            call reference_before
            call system_clock(reference_start_time, clock_rate)
            call reference
            call system_clock(reference_end_time)
            call reference_after
            reference_total_time = &
                    reference_total_time &
                    + dble(reference_end_time - reference_start_time)/dble(clock_rate)
            call before
            call system_clock(start_time)
            call computation
            call system_clock(end_time)
            call after
            total_time = total_time + dble(end_time - start_time)/dble(clock_rate)
        end do
        reference_average_time = reference_total_time / dble(iterations)
        average_time = total_time / dble(iterations)
        if (average_time < reference_average_time) then
            result__ = succeed(with_user_message( &
                    make_faster_than_success_message( &
                            to_string(reference_average_time), &
                            to_string(average_time), &
                            to_string(iterations)), &
                    success_message))
        else
            result__ = fail(with_user_message( &
                    make_faster_than_failure_message( &
                            to_string(reference_average_time), &
                            to_string(average_time), &
                            to_string(iterations)), &
                    failure_message))
        end if
    end function

    function assert_faster_than_relative_simple( &
            reference, &
            computation, &
            iterations) &
            result(result__)
        procedure(computation_i) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                computation, &
                iterations, &
                var_str(""), &
                var_str(""))
    end function

    function assert_faster_than_relative_simple_with_message_c( &
            reference, &
            computation, &
            iterations, &
            message) &
            result(result__)
        procedure(computation_i) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                computation, &
                iterations, &
                var_str(message), &
                var_str(message))
    end function

    function assert_faster_than_relative_simple_with_message_s( &
            reference, &
            computation, &
            iterations, &
            message) &
            result(result__)
        procedure(computation_i) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                computation, &
                iterations, &
                message, &
                message)
    end function

    function assert_faster_than_relative_simple_with_messages_cc( &
            reference, &
            computation, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        procedure(computation_i) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                computation, &
                iterations, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    function assert_faster_than_relative_simple_with_messages_cs( &
            reference, &
            computation, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        procedure(computation_i) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                computation, &
                iterations, &
                var_str(success_message), &
                failure_message)
    end function

    function assert_faster_than_relative_simple_with_messages_sc( &
            reference, &
            computation, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        procedure(computation_i) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_faster_than( &
                reference, &
                computation, &
                iterations, &
                success_message, &
                var_str(failure_message))
    end function

    function assert_faster_than_relative_simple_with_messages_ss( &
            reference, &
            computation, &
            iterations, &
            success_message, &
            failure_message) &
            result(result__)
        procedure(computation_i) :: reference
        procedure(computation_i) :: computation
        integer, intent(in) :: iterations
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        integer :: i
        integer(int64) :: start_time
        integer(int64) :: end_time
        double precision :: total_time
        double precision :: average_time
        integer(int64) :: reference_start_time
        integer(int64) :: reference_end_time
        integer(int64) :: clock_rate
        double precision :: reference_total_time
        double precision :: reference_average_time

        total_time = 0.0d0
        reference_total_time = 0.0d0
        do i = 1, iterations
            call system_clock(reference_start_time, clock_rate)
            call reference
            call system_clock(reference_end_time)
            reference_total_time = &
                    reference_total_time &
                    + dble(reference_end_time - reference_start_time)/dble(clock_rate)
            call system_clock(start_time)
            call computation
            call system_clock(end_time)
            total_time = total_time + dble(end_time - start_time)/dble(clock_rate)
        end do
        reference_average_time = reference_total_time / dble(iterations)
        average_time = total_time / dble(iterations)
        if (average_time < reference_average_time) then
            result__ = succeed(with_user_message( &
                    make_faster_than_success_message( &
                            to_string(reference_average_time), &
                            to_string(average_time), &
                            to_string(iterations)), &
                    success_message))
        else
            result__ = fail(with_user_message( &
                    make_faster_than_failure_message( &
                            to_string(reference_average_time), &
                            to_string(average_time), &
                            to_string(iterations)), &
                    failure_message))
        end if
    end function
end module
