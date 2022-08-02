module garden_assert_includes_m
    use iso_varying_string, only: varying_string, var_str
    use strff, only: operator(.includes.)
    use garden_messages_m, only: &
            make_includes_failure_message, &
            make_includes_success_message, &
            with_user_message
    use garden_result_m, only: result_t, fail, succeed

    implicit none
    private
    public :: assert_includes

    interface assert_includes
        module procedure assert_includes_cc
        module procedure assert_includes_cs
        module procedure assert_includes_sc
        module procedure assert_includes_ss
        module procedure assert_includes_with_message_ccc
        module procedure assert_includes_with_message_ccs
        module procedure assert_includes_with_message_csc
        module procedure assert_includes_with_message_css
        module procedure assert_includes_with_message_scc
        module procedure assert_includes_with_message_scs
        module procedure assert_includes_with_message_ssc
        module procedure assert_includes_with_message_sss
        module procedure assert_includes_with_messages_cccc
        module procedure assert_includes_with_messages_cccs
        module procedure assert_includes_with_messages_ccsc
        module procedure assert_includes_with_messages_ccss
        module procedure assert_includes_with_messages_cscc
        module procedure assert_includes_with_messages_cscs
        module procedure assert_includes_with_messages_cssc
        module procedure assert_includes_with_messages_csss
        module procedure assert_includes_with_messages_sccc
        module procedure assert_includes_with_messages_sccs
        module procedure assert_includes_with_messages_scsc
        module procedure assert_includes_with_messages_scss
        module procedure assert_includes_with_messages_sscc
        module procedure assert_includes_with_messages_sscs
        module procedure assert_includes_with_messages_sssc
        module procedure assert_includes_with_messages_ssss
    end interface
contains
    pure function assert_includes_cc( &
            search_for, &
            string) &
            result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                var_str(string), &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_includes_cs( &
            search_for, &
            string) &
            result(result__)
        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                string, &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_includes_sc( &
            search_for, &
            string) &
            result(result__)
        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                var_str(string), &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_includes_ss( &
            search_for, &
            string) &
            result(result__)
        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                string, &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_includes_with_message_ccc( &
            search_for, &
            string, &
            message) &
            result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                var_str(string), &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_includes_with_message_ccs( &
            search_for, &
            string, &
            message) &
            result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                var_str(string), &
                message, &
                message)
    end function

    pure function assert_includes_with_message_csc( &
            search_for, &
            string, &
            message) &
            result(result__)
        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                string, &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_includes_with_message_css( &
            search_for, &
            string, &
            message) &
            result(result__)
        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                string, &
                message, &
                message)
    end function

    pure function assert_includes_with_message_scc( &
            search_for, &
            string, &
            message) &
            result(result__)
        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                var_str(string), &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_includes_with_message_scs( &
            search_for, &
            string, &
            message) &
            result(result__)
        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                var_str(string), &
                message, &
                message)
    end function

    pure function assert_includes_with_message_ssc( &
            search_for, &
            string, &
            message) &
            result(result__)
        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                string, &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_includes_with_message_sss( &
            search_for, &
            string, &
            message) &
            result(result__)
        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                string, &
                message, &
                message)
    end function

    pure function assert_includes_with_messages_cccc( &
            search_for, &
            string, &
            success_message, &
            failure_message) &
            result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                var_str(string), &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_includes_with_messages_cccs( &
            search_for, &
            string, &
            success_message, &
            failure_message) &
            result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                var_str(string), &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_includes_with_messages_ccsc( &
            search_for, &
            string, &
            success_message, &
            failure_message) &
            result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                var_str(string), &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_includes_with_messages_ccss( &
            search_for, &
            string, &
            success_message, &
            failure_message) &
            result(result__)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                var_str(string), &
                success_message, &
                failure_message)
    end function

    pure function assert_includes_with_messages_cscc( &
            search_for, &
            string, &
            success_message, &
            failure_message) &
            result(result__)
        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                string, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_includes_with_messages_cscs( &
            search_for, &
            string, &
            success_message, &
            failure_message) &
            result(result__)
        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                string, &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_includes_with_messages_cssc( &
            search_for, &
            string, &
            success_message, &
            failure_message) &
            result(result__)
        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                string, &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_includes_with_messages_csss( &
            search_for, &
            string, &
            success_message, &
            failure_message) &
            result(result__)
        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                var_str(search_for), &
                string, &
                success_message, &
                failure_message)
    end function

    pure function assert_includes_with_messages_sccc( &
            search_for, &
            string, &
            success_message, &
            failure_message) &
            result(result__)
        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                var_str(string), &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_includes_with_messages_sccs( &
            search_for, &
            string, &
            success_message, &
            failure_message) &
            result(result__)
        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                var_str(string), &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_includes_with_messages_scsc( &
            search_for, &
            string, &
            success_message, &
            failure_message) &
            result(result__)
        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                var_str(string), &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_includes_with_messages_scss( &
            search_for, &
            string, &
            success_message, &
            failure_message) &
            result(result__)
        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                var_str(string), &
                success_message, &
                failure_message)
    end function

    pure function assert_includes_with_messages_sscc( &
            search_for, &
            string, &
            success_message, &
            failure_message) &
            result(result__)
        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                string, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_includes_with_messages_sscs( &
            search_for, &
            string, &
            success_message, &
            failure_message) &
            result(result__)
        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                string, &
                var_str(success_message), &
                failure_message)
    end function

    pure function assert_includes_with_messages_sssc( &
            search_for, &
            string, &
            success_message, &
            failure_message) &
            result(result__)
        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_includes( &
                search_for, &
                string, &
                success_message, &
                var_str(failure_message))
    end function

    pure function assert_includes_with_messages_ssss( &
            search_for, &
            string, &
            success_message, &
            failure_message) &
            result(result__)
        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        if (string.includes.search_for) then
            result__ = succeed(with_user_message( &
                    make_includes_success_message(search_for, string), &
                    success_message))
        else
            result__ = fail(with_user_message( &
                    make_includes_failure_message(search_for, string), &
                    failure_message))
        end if
    end function
end module
