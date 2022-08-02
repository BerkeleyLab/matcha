module garden_assert_empty_m
    use iso_varying_string, only: varying_string, operator(==), var_str
    use garden_messages_m, only: &
            make_empty_failure_message, &
            with_user_message, &
            EMPTY_SUCCESS_MESSAGE
    use garden_result_m, only: result_t, fail, succeed

    implicit none
    private
    public :: assert_empty

    interface assert_empty
        module procedure assert_empty_basic_c
        module procedure assert_empty_basic_s
        module procedure assert_empty_with_message_cc
        module procedure assert_empty_with_message_cs
        module procedure assert_empty_with_message_sc
        module procedure assert_empty_with_message_ss
        module procedure assert_empty_with_messages_ccc
        module procedure assert_empty_with_messages_ccs
        module procedure assert_empty_with_messages_csc
        module procedure assert_empty_with_messages_css
        module procedure assert_empty_with_messages_scc
        module procedure assert_empty_with_messages_scs
        module procedure assert_empty_with_messages_ssc
        module procedure assert_empty_with_messages_sss
    end interface
contains
    pure function assert_empty_basic_c(string) result(result__)
        character(len=*), intent(in) :: string
        type(result_t) :: result__

        result__ = assert_empty( &
                var_str(string), &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_empty_basic_s(string) result(result__)
        type(varying_string), intent(in) :: string
        type(result_t) :: result__

        result__ = assert_empty( &
                string, &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_empty_with_message_cc(string, message) result(result__)
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_empty( &
                var_str(string), &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_empty_with_message_cs(string, message) result(result__)
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_empty( &
                var_str(string), &
                message, &
                message)
    end function

    pure function assert_empty_with_message_sc(string, message) result(result__)
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_empty( &
                string, &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_empty_with_message_ss(string, message) result(result__)
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_empty( &
                string, &
                message, &
                message)
    end function

    pure function assert_empty_with_messages_ccc( &
            string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_empty( &
                var_str(string), &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_empty_with_messages_ccs( &
            string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_empty( &
                var_str(string), &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_empty_with_messages_csc( &
            string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_empty( &
                var_str(string), &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_empty_with_messages_css( &
            string, success_message, failure_message) result(result__)
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_empty( &
                var_str(string), &
                success_message, &
                failure_message)
    end function

    pure function assert_empty_with_messages_scc( &
            string, success_message, failure_message) result(result__)
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_empty( &
                string, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_empty_with_messages_scs( &
            string, success_message, failure_message) result(result__)
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_empty( &
                string, &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_empty_with_messages_ssc( &
            string, success_message, failure_message) result(result__)
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_empty( &
                string, &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_empty_with_messages_sss( &
            string, success_message, failure_message) result(result__)
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        if (string == "") then
            result__ = succeed(with_user_message( &
                    EMPTY_SUCCESS_MESSAGE, success_message))
        else
            result__ = fail(with_user_message( &
                    make_empty_failure_message(string), failure_message))
        end if
    end function
end module
