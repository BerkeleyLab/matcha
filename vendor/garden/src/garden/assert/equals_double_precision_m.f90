module garden_assert_equals_double_precision_m
    use iso_varying_string, only: varying_string, var_str
    use garden_assert_equals_within_absolute_m, only: &
            assert_equals_within_absolute
    use garden_result_m, only: result_t

    implicit none
    private
    public :: assert_equals

    interface assert_equals
        module procedure assert_equals_double_precision
        module procedure assert_equals_double_precision_with_message_c
        module procedure assert_equals_double_precision_with_message_s
        module procedure assert_equals_double_precision_with_messages_cc
        module procedure assert_equals_double_precision_with_messages_cs
        module procedure assert_equals_double_precision_with_messages_sc
        module procedure assert_equals_double_precision_with_messages_ss
    end interface
contains
    pure function assert_equals_double_precision( &
            expected, &
            actual) &
            result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_equals_double_precision_with_message_c( &
            expected, &
            actual, &
            message) &
            result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, actual, var_str(message), var_str(message))
    end function

    pure function assert_equals_double_precision_with_message_s( &
            expected, &
            actual, &
            message) &
            result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals(expected, actual, message, message)
    end function

    pure function assert_equals_double_precision_with_messages_cc( &
            expected, &
            actual, &
            success_message, &
            failure_message) &
            result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_equals_double_precision_with_messages_cs( &
            expected, &
            actual, &
            success_message, &
            failure_message) &
            result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, actual, var_str(success_message), failure_message)
    end function

    pure function assert_equals_double_precision_with_messages_sc( &
            expected, &
            actual, &
            success_message, &
            failure_message) &
            result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, actual, success_message, var_str(failure_message))
    end function

    pure function assert_equals_double_precision_with_messages_ss( &
            expected, &
            actual, &
            success_message, &
            failure_message) &
            result(result__)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        double precision, parameter :: MACHINE_EPSILON = epsilon(0.0d0)

        result__ = assert_equals_within_absolute( &
                expected, &
                actual, &
                MACHINE_EPSILON, &
                success_message, &
                failure_message)
    end function
end module
