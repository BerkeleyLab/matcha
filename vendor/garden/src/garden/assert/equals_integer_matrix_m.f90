module garden_assert_equals_integer_matrix_m
    use iso_varying_string, only: varying_string, operator(//), var_str
    use garden_messages_m, only: &
            make_equals_failure_message, &
            make_equals_success_message, &
            with_user_message
    use garden_result_m, only: result_t, fail, succeed
    use garden_utilities_m, only: to_string

    implicit none
    private
    public :: assert_equals

    interface assert_equals
        module procedure assert_equals_integer_matrix_basic
        module procedure assert_equals_integer_matrix_with_message_c
        module procedure assert_equals_integer_matrix_with_message_s
        module procedure assert_equals_integer_matrix_with_messages_cc
        module procedure assert_equals_integer_matrix_with_messages_cs
        module procedure assert_equals_integer_matrix_with_messages_sc
        module procedure assert_equals_integer_matrix_with_messages_ss
    end interface
contains
    pure function assert_equals_integer_matrix_basic(expected, actual) result(result__)
        integer, intent(in) :: expected(:,:)
        integer, intent(in) :: actual(:,:)
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                var_str(""), &
                var_str(""))
    end function

    pure function assert_equals_integer_matrix_with_message_c( &
            expected, actual, message) result(result__)
        integer, intent(in) :: expected(:,:)
        integer, intent(in) :: actual(:,:)
        character(len=*), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                var_str(message), &
                var_str(message))
    end function

    pure function assert_equals_integer_matrix_with_message_s( &
            expected, actual, message) result(result__)
        integer, intent(in) :: expected(:,:)
        integer, intent(in) :: actual(:,:)
        type(varying_string), intent(in) :: message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                message, &
                message)
    end function

    pure function assert_equals_integer_matrix_with_messages_cc( &
            expected, actual, success_message, failure_message) result(result__)
        integer, intent(in) :: expected(:,:)
        integer, intent(in) :: actual(:,:)
        character(len=*), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, &
                actual, &
                var_str(success_message), &
                var_str(failure_message))
    end function

    pure function assert_equals_integer_matrix_with_messages_cs( &
            expected, actual, success_message, failure_message) result(result__)
        integer, intent(in) :: expected(:,:)
        integer, intent(in) :: actual(:,:)
        character(len=*), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, actual, var_str(success_message), failure_message)
    end function

    pure function assert_equals_integer_matrix_with_messages_sc( &
            expected, actual, success_message, failure_message) result(result__)
        integer, intent(in) :: expected(:,:)
        integer, intent(in) :: actual(:,:)
        type(varying_string), intent(in) :: success_message
        character(len=*), intent(in) :: failure_message
        type(result_t) :: result__

        result__ = assert_equals( &
                expected, actual, success_message, var_str(failure_message))
    end function

    pure function assert_equals_integer_matrix_with_messages_ss( &
            expected, actual, success_message, failure_message) result(result__)
        integer, intent(in) :: expected(:,:)
        integer, intent(in) :: actual(:,:)
        type(varying_string), intent(in) :: success_message
        type(varying_string), intent(in) :: failure_message
        type(result_t) :: result__

        if ( &
                size(expected, dim=1) == size(actual, dim=1) &
                .and. size(expected, dim=2) == size(actual, dim=2)) then
            if (all(expected == actual)) then
                result__ = succeed(with_user_message( &
                        make_equals_success_message(to_string(expected)), &
                        success_message))
                return
            end if
        end if
        result__ = fail(with_user_message( &
                make_equals_failure_message( &
                        to_string(expected), &
                        to_string(actual)), &
                failure_message))
    end function
end module
