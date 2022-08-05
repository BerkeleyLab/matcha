module garden_assert_that_m
    use iso_varying_string, only: varying_string, var_str
    use garden_messages_m, only: &
            with_user_message, THAT_FAILURE_MESSAGE, THAT_SUCCESS_MESSAGE
    use garden_result_m, only: result_t, fail, succeed

    implicit none
    private
    public :: assert_that

    interface assert_that
        module procedure assert_that_basic
        module procedure assert_that_with_message_c
        module procedure assert_that_with_message_s
        module procedure assert_that_with_messages_cc
        module procedure assert_that_with_messages_cs
        module procedure assert_that_with_messages_sc
        module procedure assert_that_with_messages_ss
    end interface
contains
    pure function assert_that_basic(condition) result(result__)
        logical, intent(in) :: condition
        type(result_t) :: result__

        result__ = assert_that(condition, var_str(""), var_str(""))
    end function

    pure function assert_that_with_message_c(condition, message) result(result__)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_that(condition, var_str(message), var_str(message))
    end function

    pure function assert_that_with_message_s(condition, message) result(result__)
        logical, intent(in) :: condition
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_that(condition, message, message)
    end function

    pure function assert_that_with_messages_cc( &
            condition, success_message, failure_message) result(result__)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_that( &
                condition, var_str(success_message), var_str(failure_message))
    end function

    pure function assert_that_with_messages_cs( &
            condition, success_message, failure_message) result(result__)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_that( &
                condition, var_str(success_message), failure_message)
    end function

    pure function assert_that_with_messages_sc( &
            condition, success_message, failure_message) result(result__)
        logical, intent(in) :: condition
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_that( &
                condition, success_message, var_str(failure_message))
    end function

    pure function assert_that_with_messages_ss( &
            condition, success_message, failure_message) result(result__)
        logical, intent(in) :: condition
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        if (condition) then
            result__ = succeed(with_user_message( &
                    THAT_SUCCESS_MESSAGE, success_message))
        else
            result__ = fail(with_user_message( &
                    THAT_FAILURE_MESSAGE, failure_message))
        end if
    end function
end module
