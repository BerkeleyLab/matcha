module garden_assert_equals_integer_m
    use iso_varying_string, only: varying_string, var_str
    use strff, only: to_string
    use garden_messages_m, only: &
            make_equals_failure_message, &
            make_equals_success_message, &
            with_user_message
    use garden_result_m, only: result_t, fail, succeed

    implicit none
    private
    public :: assert_equals

    interface assert_equals
        module procedure assert_equals_integer_basic
        module procedure assert_equals_integer_with_message_c
        module procedure assert_equals_integer_with_message_s
        module procedure assert_equals_integer_with_messages_cc
        module procedure assert_equals_integer_with_messages_cs
        module procedure assert_equals_integer_with_messages_sc
        module procedure assert_equals_integer_with_messages_ss
    end interface
contains
    pure function assert_equals_integer_basic(expected, actual) result(result__)
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_equals_integer_with_message_c( &
            expected, actual, message) result(result__)
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_equals_integer_with_message_s( &
            expected, actual, message) result(result__)
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                message, &
                message)
    end function

    pure function assert_equals_integer_with_messages_cc( &
            expected, actual, success_message, failure_message) result(result__)
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_equals_integer_with_messages_cs( &
            expected, actual, success_message, failure_message) result(result__)
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, actual, var_str(success_message), failure_message)
    end function

    pure function assert_equals_integer_with_messages_sc( &
            expected, actual, success_message, failure_message) result(result__)
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, actual, success_message, var_str(failure_message))
    end function

    pure function assert_equals_integer_with_messages_ss( &
            expected, actual, success_message, failure_message) result(result__)
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        if (expected == actual) then
            result__ = succeed(with_user_message( &
                    make_equals_success_message(to_string(expected)), &
                    success_message))
        else
            result__ = fail(with_user_message( &
                    make_equals_failure_message( &
                            to_string(expected), to_string(actual)), &
                    failure_message))
        end if
    end function
end module
