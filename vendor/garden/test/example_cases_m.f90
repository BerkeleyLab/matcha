module example_cases_m
    use example_asserts_m, only: &
            example_multiple_asserts, example_multiple_asserts_with_fail
    use garden, only: test_item_t, it

    implicit none
    private
    public :: &
            example_failing_test_case, &
            example_passing_test_case, &
            EXAMPLE_DESCRIPTION, &
            NOT_IN_DESCRIPTION

    character(len=*), parameter :: EXAMPLE_DESCRIPTION = "Example Description"
    character(len=*), parameter :: NOT_IN_DESCRIPTION = "NOT"
contains
    function example_passing_test_case() result(test_case)
        type(test_item_t) :: test_case

        test_case = it(EXAMPLE_DESCRIPTION, example_multiple_asserts)
    end function

    function example_failing_test_case() result(test_case)
        type(test_item_t) :: test_case

        test_case = it(EXAMPLE_DESCRIPTION, example_multiple_asserts_with_fail)
    end function
end module
