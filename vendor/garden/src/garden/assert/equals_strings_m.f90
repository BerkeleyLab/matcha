module garden_assert_equals_strings_m
    use iso_varying_string, only: varying_string, operator(==), var_str
    use garden_messages_m, only: &
            make_equals_failure_message, &
            make_equals_success_message, &
            with_user_message
    use garden_result_m, only: result_t, fail, succeed

    implicit none
    private
    public :: assert_equals

    interface assert_equals
        module procedure assert_equals_strings_cc
        module procedure assert_equals_strings_cs
        module procedure assert_equals_strings_sc
        module procedure assert_equals_strings_ss
        module procedure assert_equals_strings_with_message_ccc
        module procedure assert_equals_strings_with_message_ccs
        module procedure assert_equals_strings_with_message_csc
        module procedure assert_equals_strings_with_message_css
        module procedure assert_equals_strings_with_message_scc
        module procedure assert_equals_strings_with_message_scs
        module procedure assert_equals_strings_with_message_ssc
        module procedure assert_equals_strings_with_message_sss
        module procedure assert_equals_strings_with_messages_cccc
        module procedure assert_equals_strings_with_messages_cccs
        module procedure assert_equals_strings_with_messages_ccsc
        module procedure assert_equals_strings_with_messages_ccss
        module procedure assert_equals_strings_with_messages_cscc
        module procedure assert_equals_strings_with_messages_cscs
        module procedure assert_equals_strings_with_messages_cssc
        module procedure assert_equals_strings_with_messages_csss
        module procedure assert_equals_strings_with_messages_sccc
        module procedure assert_equals_strings_with_messages_sccs
        module procedure assert_equals_strings_with_messages_scsc
        module procedure assert_equals_strings_with_messages_scss
        module procedure assert_equals_strings_with_messages_sscc
        module procedure assert_equals_strings_with_messages_sscs
        module procedure assert_equals_strings_with_messages_sssc
        module procedure assert_equals_strings_with_messages_ssss
    end interface
contains
    pure function assert_equals_strings_cc( &
            expected, actual) result(result__)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                var_str(actual), &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_equals_strings_cs( &
            expected, actual) result(result__)
        character(len=*), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                actual, &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_equals_strings_sc( &
            expected, actual) result(result__)
        type(varying_string), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                var_str(actual), &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_equals_strings_ss( &
            expected, actual) result(result__)
        type(varying_string), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_equals_strings_with_message_ccc( &
            expected, actual, message) result(result__)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                var_str(actual), &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_equals_strings_with_message_ccs( &
            expected, actual, message) result(result__)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                var_str(actual), &
                message, &
                message)
    end function

    pure function assert_equals_strings_with_message_csc( &
            expected, actual, message) result(result__)
        character(len=*), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                actual, &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_equals_strings_with_message_css( &
            expected, actual, message) result(result__)
        character(len=*), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                actual, &
                message, &
                message)
    end function

    pure function assert_equals_strings_with_message_scc( &
            expected, actual, message) result(result__)
        type(varying_string), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                var_str(actual), &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_equals_strings_with_message_scs( &
            expected, actual, message) result(result__)
        type(varying_string), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                var_str(actual), &
                message, &
                message)
    end function

    pure function assert_equals_strings_with_message_ssc( &
            expected, actual, message) result(result__)
        type(varying_string), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_equals_strings_with_message_sss( &
            expected, actual, message) result(result__)
        type(varying_string), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                message, &
                message)
    end function

    pure function assert_equals_strings_with_messages_cccc( &
            expected, actual, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                var_str(actual), &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_equals_strings_with_messages_cccs( &
            expected, actual, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                var_str(actual), &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_equals_strings_with_messages_ccsc( &
            expected, actual, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                var_str(actual), &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_equals_strings_with_messages_ccss( &
            expected, actual, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                var_str(actual), &
                success_message, &
                failure_message)
    end function

    pure function assert_equals_strings_with_messages_cscc( &
            expected, actual, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                actual, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_equals_strings_with_messages_cscs( &
            expected, actual, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                actual, &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_equals_strings_with_messages_cssc( &
            expected, actual, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                actual, &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_equals_strings_with_messages_csss( &
            expected, actual, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                var_str(expected), &
                actual, &
                success_message, &
                failure_message)
    end function

    pure function assert_equals_strings_with_messages_sccc( &
            expected, actual, success_message, failure_message) result(result__)
        type(varying_string), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                var_str(actual), &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_equals_strings_with_messages_sccs( &
            expected, actual, success_message, failure_message) result(result__)
        type(varying_string), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                var_str(actual), &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_equals_strings_with_messages_scsc( &
            expected, actual, success_message, failure_message) result(result__)
        type(varying_string), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                var_str(actual), &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_equals_strings_with_messages_scss( &
            expected, actual, success_message, failure_message) result(result__)
        type(varying_string), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                var_str(actual), &
                success_message, &
                failure_message)
    end function

    pure function assert_equals_strings_with_messages_sscc( &
            expected, actual, success_message, failure_message) result(result__)
        type(varying_string), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_equals_strings_with_messages_sscs( &
            expected, actual, success_message, failure_message) result(result__)
        type(varying_string), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_equals_strings_with_messages_sssc( &
            expected, actual, success_message, failure_message) result(result__)
        type(varying_string), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_equals_strings_with_messages_ssss( &
            expected, actual, success_message, failure_message) result(result__)
        type(varying_string), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        if (expected == actual) then
            result__ = succeed(with_user_message( &
                    make_equals_success_message(expected), &
                    success_message))
        else
            result__ = fail(with_user_message( &
                    make_equals_failure_message(expected, actual), &
                    failure_message))
        end if
    end function
end module
