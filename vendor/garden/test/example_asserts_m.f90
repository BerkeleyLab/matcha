module example_asserts_m
    use garden, only: result_t, fail, succeed

    implicit none
    private
    public :: &
            example_multiple_asserts, &
            example_multiple_asserts_with_fail, &
            FAILURE_MESSAGE, &
            NUM_ASSERTS_IN_FAILING, &
            NUM_ASSERTS_IN_PASSING, &
            NUM_FAILING_ASSERTS_IN_FAILING, &
            NUM_PASSING_ASSERTS_IN_FAILING, &
            SUCCESS_MESSAGE

    character(len=*), parameter :: FAILURE_MESSAGE = "Failure Message"
    integer, parameter :: NUM_FAILING_ASSERTS_IN_FAILING = 1
    integer, parameter :: NUM_PASSING_ASSERTS_IN_FAILING = 1
    integer, parameter :: NUM_ASSERTS_IN_FAILING = &
            NUM_FAILING_ASSERTS_IN_FAILING + NUM_PASSING_ASSERTS_IN_FAILING
    integer, parameter :: NUM_ASSERTS_IN_PASSING = 2
    character(len=*), parameter :: SUCCESS_MESSAGE = "Success Message"
contains
    pure function example_multiple_asserts() result(result_)
        type(result_t) :: result_

        result_ = succeed(SUCCESS_MESSAGE).and.succeed(SUCCESS_MESSAGE)
    end function

    pure function example_multiple_asserts_with_fail() result(result_)
        type(result_t) :: result_

        result_ = succeed(SUCCESS_MESSAGE).and.fail(FAILURE_MESSAGE)
    end function
end module
