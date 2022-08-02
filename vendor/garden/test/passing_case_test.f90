module passing_case_test
    use example_asserts_m, only: NUM_ASSERTS_IN_PASSING, SUCCESS_MESSAGE
    use example_cases_m, only: example_passing_test_case, EXAMPLE_DESCRIPTION
    use helpers_m, only: test_item_input_t, test_result_item_input_t, run_test
    use garden, only: &
            input_t, &
            result_t, &
            test_item_t, &
            test_result_item_t, &
            assert_empty, &
            assert_equals, &
            assert_includes, &
            assert_that, &
            fail, &
            given, &
            then__, &
            when

    implicit none
    private

    public :: test_passing_case_behaviors
contains
    function test_passing_case_behaviors() result(test)
        type(test_item_t) :: test

        test = given( &
                "a passing test case", &
                test_item_input_t(example_passing_test_case()), &
                [ when( &
                        "it is run", &
                        run_test, &
                        [ then__("it knows it passed", check_case_passes) &
                        , then__("it has 1 test case", check_num_cases) &
                        , then__("it has no failing case", check_num_failing_cases) &
                        , then__( &
                                "it's verbose description still includes the given description", &
                                check_verbose_description) &
                        , then__( &
                                "it's verbose description includes the assertion message", &
                                check_verbose_description_assertion) &
                        , then__( &
                                "it's failure description is empty", &
                                check_failure_description_empty) &
                        , then__("it knows how many asserts there were", check_num_asserts) &
                        , then__("it has no failing asserts", check_num_failing_asserts) &
                        ]) &
                ])
    end function

    function check_case_passes(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_result

        select type (input)
        type is (test_result_item_input_t)
            example_result = input%input()
            result_ = assert_that(example_result%passed())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_num_cases(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_result

        select type (input)
        type is (test_result_item_input_t)
            example_result = input%input()
            result_ = assert_equals(1, example_result%num_cases())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_num_failing_cases(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_result

        select type (input)
        type is (test_result_item_input_t)
            example_result = input%input()
            result_ = assert_equals(0, example_result%num_failing_cases())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_verbose_description(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_result

        select type (input)
        type is (test_result_item_input_t)
            example_result = input%input()
            result_ = assert_includes(EXAMPLE_DESCRIPTION, example_result%verbose_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_verbose_description_assertion(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_result

        select type (input)
        type is (test_result_item_input_t)
            example_result = input%input()
            result_ = assert_includes(SUCCESS_MESSAGE, example_result%verbose_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_failure_description_empty(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_result

        select type (input)
        type is (test_result_item_input_t)
            example_result = input%input()
            result_ = assert_empty(example_result%failure_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_num_asserts(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_result

        select type (input)
        type is (test_result_item_input_t)
            example_result = input%input()
            result_ = assert_equals(NUM_ASSERTS_IN_PASSING, example_result%num_asserts())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_num_failing_asserts(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_result

        select type (input)
        type is (test_result_item_input_t)
            example_result = input%input()
            result_ = assert_equals(0, example_result%num_failing_asserts())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function
end module
