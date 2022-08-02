module passing_collection_test
    use example_asserts_m, only: SUCCESS_MESSAGE
    use example_collections_m, only: &
            example_passing_collection, &
            EXAMPLE_CASE_DESCRIPTION_1, &
            EXAMPLE_CASE_DESCRIPTION_2, &
            EXAMPLE_COLLECTION_DESCRIPTION, &
            NUM_ASSERTS_IN_PASSING, &
            NUM_CASES_IN_PASSING
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

    public :: test_passing_collection_behaviors
contains
    function test_passing_collection_behaviors() result(tests)
        type(test_item_t) :: tests

        tests = given( &
                "a passing test collection", &
                test_item_input_t(example_passing_collection()), &
                [ when( &
                        "it is run", &
                        run_test, &
                        [ then__("it knows it passed", check_collection_passes) &
                        , then__("it knows how many cases there were", check_num_cases) &
                        , then__("it has no failing cases", check_num_failing_cases) &
                        , then__( &
                                "it's verbose description includes the given description", &
                                check_werbose_top_description) &
                        , then__( &
                                "it's verbose description includes the individual case descriptions", &
                                check_verbose_case_descriptions) &
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

    function check_collection_passes(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_results

        select type (input)
        type is (test_result_item_input_t)
            example_results = input%input()
            result_ = assert_that(example_results%passed(), "It passed", "It didn't pass")
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_num_cases(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_results

        select type (input)
        type is (test_result_item_input_t)
            example_results = input%input()
            result_ = assert_equals(NUM_CASES_IN_PASSING, example_results%num_cases())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_num_failing_cases(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_results

        select type (input)
        type is (test_result_item_input_t)
            example_results = input%input()
            result_ = assert_equals(0, example_results%num_failing_cases())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_werbose_top_description(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_results

        select type (input)
        type is (test_result_item_input_t)
            example_results = input%input()
            result_ = assert_includes( &
                    EXAMPLE_COLLECTION_DESCRIPTION, &
                    example_results%verbose_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_verbose_case_descriptions(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_results

        select type (input)
        type is (test_result_item_input_t)
            example_results = input%input()
            result_ = &
                    assert_includes( &
                            EXAMPLE_CASE_DESCRIPTION_1, &
                            example_results%verbose_description(.false.)) &
                    .and.assert_includes( &
                            EXAMPLE_CASE_DESCRIPTION_2, &
                            example_results%verbose_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_verbose_description_assertion(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_results

        select type (input)
        type is (test_result_item_input_t)
            example_results = input%input()
            result_ = assert_includes( &
                    SUCCESS_MESSAGE, &
                    example_results%verbose_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_failure_description_empty(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_results

        select type (input)
        type is (test_result_item_input_t)
            example_results = input%input()
            result_ = assert_empty(example_results%failure_description(.false.))
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_num_asserts(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_results

        select type (input)
        type is (test_result_item_input_t)
            example_results = input%input()
            result_ = assert_equals(NUM_ASSERTS_IN_PASSING, example_results%num_asserts())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function

    function check_num_failing_asserts(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_result_item_t) :: example_results

        select type (input)
        type is (test_result_item_input_t)
            example_results = input%input()
            result_ = assert_equals(0, example_results%num_failing_asserts())
        class default
            result_ = fail("Expected to get a test_result_item_input_t")
        end select
    end function
end module
