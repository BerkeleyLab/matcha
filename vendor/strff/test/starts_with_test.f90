module starts_with_test
    use strff, only: operator(.startswith.)
    use veggies, only: &
            test_item_t, result_t, assert_not, assert_that, describe, it

    implicit none
    private

    public :: test_starts_with
contains
    function test_starts_with() result(tests)
        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(2)

        individual_tests(1) = it( &
                "is true if the first string starts with the second", &
                check_true)
        individual_tests(2) = it( &
                "is false if the first string doesn't start with the second", &
                check_false)
        tests = describe(".startswith.", individual_tests)
    end function

    pure function check_true() result(result_)
        type(result_t) :: result_

        result_ = assert_that("Hello, World!".startswith."Hello")
    end function

    pure function check_false() result(result_)
        type(result_t) :: result_

        result_ = assert_not("Hello, World!".startswith."World!")
    end function
end module
