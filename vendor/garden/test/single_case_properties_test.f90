module single_case_properties_test
    use example_asserts_m, only: example_multiple_asserts
    use example_cases_m, only: example_passing_test_case, EXAMPLE_DESCRIPTION
    use helpers_m, only: test_item_input_t
    use garden, only: &
            input_t, &
            result_t, &
            test_item_t, &
            test_result_item_t, &
            assert_equals, &
            assert_faster_than, &
            assert_includes, &
            describe, &
            fail, &
            it_

    implicit none
    private

    public :: test_case_properties
contains
    function test_case_properties() result(test)
        type(test_item_t) :: test

        test = describe( &
                "A test case", &
                test_item_input_t(example_passing_test_case()), &
                [ it_("includes the given description", check_case_description) &
                , it_("only has 1 test case", check_num_cases) &
                , it_( &
                        "takes less than 3 times as long as the assertions to run", &
                        check_speed) &
                ])
    end function

    function check_case_description(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_item_t) :: example_case

        select type (input)
        class is (test_item_input_t)
            example_case = input%input()
            result_ = assert_includes(EXAMPLE_DESCRIPTION, example_case%description())
        class default
            result_ = fail("Expected to get a test_item_input_t")
        end select
    end function

    function check_num_cases(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_item_t) :: example_case

        select type (input)
        class is (test_item_input_t)
            example_case = input%input()
            result_ = assert_equals(1, example_case%num_cases())
        class default
            result_ = fail("Expected to get a test_item_input_t")
        end select
    end function

    function check_speed(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_item_t) :: internal_case

        select type (input)
        type is (test_item_input_t)
            internal_case = input%input()
            result_ = assert_faster_than(run_assertions, run_case, 100)
        class default
            result_ = fail("Expected to get a test_item_input_t")
        end select
    contains
        subroutine run_case
            integer :: i
            type(test_result_item_t) :: internal_result

            do i = 1, 100
                internal_result = internal_case%run()
            end do
        end subroutine

        subroutine run_assertions
            integer :: i
            type(result_t) :: result__

            do i = 1, 300
                result__ = example_multiple_asserts()
            end do
        end subroutine
    end function
end module
