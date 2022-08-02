module garden_assert_equals_within_absolute_m
    use iso_varying_string, only: varying_string, var_str
    use strff, only: to_string
    use garden_messages_m, only: &
            make_within_failure_message, &
            make_within_success_message, &
            with_user_message
    use garden_result_m, only: result_t, fail, succeed
    use garden_utilities_m, only: equals_within_absolute

    implicit none
    private
    public :: assert_equals_within_absolute

    interface assert_equals_within_absolute
        module procedure assert_equals_within_absolute_basic
        module procedure assert_equals_within_absolute_with_message_c
        module procedure assert_equals_within_absolute_with_message_s
        module procedure assert_equals_within_absolute_with_messages_cc
        module procedure assert_equals_within_absolute_with_messages_cs
        module procedure assert_equals_within_absolute_with_messages_sc
        module procedure assert_equals_within_absolute_with_messages_ss
    end interface
contains
    pure function assert_equals_within_absolute_basic( &
            expected, &
            actual, &
            tolerance) &
            result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(result_t) :: result__

        result__ = assert_equals_within_absolute( &
                expected, &
                actual, &
                tolerance, &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_equals_within_absolute_with_message_c( &
            expected, &
            actual, &
            tolerance, &
            message) &
            result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals_within_absolute( &
                expected, &
                actual, &
                tolerance, &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_equals_within_absolute_with_message_s( &
            expected, &
            actual, &
            tolerance, &
            message) &
            result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals_within_absolute( &
                expected, &
                actual, &
                tolerance, &
                message, &
                message)
    end function

    pure function assert_equals_within_absolute_with_messages_cc( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals_within_absolute( &
                expected, &
                actual, &
                tolerance, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_equals_within_absolute_with_messages_cs( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals_within_absolute( &
                expected, &
                actual, &
                tolerance, &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_equals_within_absolute_with_messages_sc( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals_within_absolute( &
                expected, &
                actual, &
                tolerance, &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_equals_within_absolute_with_messages_ss( &
            expected, &
            actual, &
            tolerance, &
            success_message, &
            failure_message) &
            result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        if (equals_within_absolute(expected, actual, tolerance)) then
            result__ = succeed(with_user_message( &
                    make_within_success_message( &
                            to_string(expected), &
                            to_string(actual), &
                            to_string(tolerance)), &
                    success_message))
        else
            result__ = fail(with_user_message( &
                    make_within_failure_message( &
                            to_string(expected), &
                            to_string(actual), &
                            to_string(tolerance)), &
                    failure_message))
        end if
    end function
end module
