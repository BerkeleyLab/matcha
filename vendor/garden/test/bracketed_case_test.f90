module bracketed_case_test
    use garden, only: &
            result_t, &
            test_item_t, &
            test_result_item_t, &
            assert_equals, &
            describe, &
            it, &
            succeed

    implicit none
    private
    public :: test_bracketed_test_case
contains
    function test_bracketed_test_case() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "A bracketed test case", &
                [ it("executes the setup routine once", check_setup) &
                , it("executes the teardown routine once", check_teardown) &
                ])
    end function

    function check_setup() result(result_)
        type(result_t) :: result_

        type(test_item_t) :: example_case
        type(test_result_item_t) :: example_result
        integer :: times_called

        times_called = 0
        example_case = it("is bracketed", dummy, setup, teardown)
        example_result = example_case%run()
        result_ = assert_equals(1, times_called)
    contains
        subroutine setup
            times_called = times_called + 1
        end subroutine

        subroutine teardown
        end subroutine
    end function

    function check_teardown() result(result_)
        type(result_t) :: result_

        type(test_item_t) :: example_case
        type(test_result_item_t) :: example_result
        integer :: times_called

        times_called = 0
        example_case = it("is bracketed", dummy, setup, teardown)
        example_result = example_case%run()
        result_ = assert_equals(1, times_called)
    contains
        subroutine setup
        end subroutine

        subroutine teardown
            times_called = times_called + 1
        end subroutine
    end function

    function dummy() result(result_)
        type(result_t) :: result_

        result_ = succeed("for example")
    end function
end module
