module filter_test
    use example_cases_m, only: &
            example_passing_test_case, EXAMPLE_DESCRIPTION, NOT_IN_DESCRIPTION
    use example_collections_m, only: &
            example_passing_collection, &
            EXAMPLE_CASE_DESCRIPTION_1, &
            EXAMPLE_COLLECTION_DESCRIPTION, &
            NOT_IN_DESCRIPTIONS
    use helpers_m, only: test_item_input_t
    use iso_varying_string, only: var_str
    use garden, only: &
            input_t, &
            filter_item_result_t, &
            result_t, &
            test_item_t, &
            transformation_failure_t, &
            transformed_t, &
            assert_equals, &
            assert_not, &
            fail, &
            given, &
            then__, &
            when

    implicit none
    private
    public :: test_filter_case, test_filter_collection

    type, extends(input_t) :: filter_item_result_input_t
        private
        type(filter_item_result_t) :: input_
    contains
        private
        procedure, public :: input
    end type

    interface filter_item_result_input_t
        module procedure constructor
    end interface
contains
    function test_filter_case() result(tests)
        type(test_item_t) :: tests

        tests = given( &
                "a test case", &
                test_item_input_t(example_passing_test_case()), &
                [ when( &
                        "it is filtered with a string it doesn't contain", &
                        filter_case_not_matching, &
                        [then__("it doesn't match", check_case_not_matching)]) &
                , when( &
                        "it is filtered with a matching string", &
                        filter_case_matching, &
                        [then__("it returns itself", check_case_is_same)]) &
                ])
    end function

    function test_filter_collection() result(tests)
        type(test_item_t) :: tests

        tests = given( &
                "a test collection", &
                test_item_input_t(example_passing_collection()), &
                [ when( &
                        "it is filtered with a string it doesn't contain", &
                        filter_collection_not_matching, &
                        [then__("it doesn't match", check_collection_not_matching)]) &
                , when( &
                        "it is filtered with a string matching its description", &
                        filter_collection_matching_description, &
                        [then__("it returns itself", check_collection_is_same)]) &
                , when( &
                        "it is filtered with a string matching only 1 of its cases", &
                        filter_collection_matching_case, &
                        [ then__( &
                                "it returns a collection with only that case", &
                                check_collection_single_case) &
                        ]) &
                ])
    end function

    function filter_case_not_matching(input) result(filtered)
        class(input_t), intent(in) :: input
        type(transformed_t) :: filtered

        type(test_item_t) :: example_case

        select type (input)
        type is (test_item_input_t)
            example_case = input%input()
            filtered = transformed_t(filter_item_result_input_t( &
                    example_case%filter(var_str(NOT_IN_DESCRIPTION))))
        class default
            filtered = transformed_t(transformation_failure_t( &
                    fail("Expected to get a test_item_input_t")))
        end select
    end function

    function filter_case_matching(input) result(filtered)
        class(input_t), intent(in) :: input
        type(transformed_t) :: filtered

        type(test_item_t) :: example_case

        select type (input)
        type is (test_item_input_t)
            example_case = input%input()
            filtered = transformed_t(filter_item_result_input_t( &
                    example_case%filter(var_str(EXAMPLE_DESCRIPTION))))
        class default
            filtered = transformed_t(transformation_failure_t( &
                    fail("Expected to get a test_item_input_t")))
        end select
    end function

    function filter_collection_not_matching(input) result(filtered)
        class(input_t), intent(in) :: input
        type(transformed_t) :: filtered

        type(test_item_t) :: example_collection

        select type (input)
        type is (test_item_input_t)
            example_collection = input%input()
            filtered = transformed_t(filter_item_result_input_t( &
                    example_collection%filter(var_str(NOT_IN_DESCRIPTIONS))))
        class default
            filtered = transformed_t(transformation_failure_t( &
                    fail("Expected to get a test_item_input_t")))
        end select
    end function

    function filter_collection_matching_description(input) result(filtered)
        class(input_t), intent(in) :: input
        type(transformed_t) :: filtered

        type(test_item_t) :: example_collection

        select type (input)
        type is (test_item_input_t)
            example_collection = input%input()
            filtered = transformed_t(filter_item_result_input_t( &
                    example_collection%filter(var_str(EXAMPLE_COLLECTION_DESCRIPTION))))
        class default
            filtered = transformed_t(transformation_failure_t( &
                    fail("Expected to get a test_item_input_t")))
        end select
    end function

    function filter_collection_matching_case(input) result(filtered)
        class(input_t), intent(in) :: input
        type(transformed_t) :: filtered

        type(test_item_t) :: example_collection

        select type (input)
        type is (test_item_input_t)
            example_collection = input%input()
            filtered = transformed_t(filter_item_result_input_t( &
                    example_collection%filter(var_str(EXAMPLE_CASE_DESCRIPTION_1))))
        class default
            filtered = transformed_t(transformation_failure_t( &
                    fail("Expected to get a test_item_input_t")))
        end select
    end function

    function check_case_not_matching(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(filter_item_result_t) :: filtered

        select type (input)
        type is (filter_item_result_input_t)
            filtered = input%input()
            result_ = assert_not(filtered%matched())
        class default
            result_ = fail("Expected to get filter_item_result_input_t")
        end select
    end function

    function check_case_is_same(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(filter_item_result_t) :: filtered
        type(test_item_t) :: test_item

        select type (input)
        type is (filter_item_result_input_t)
            filtered = input%input()
            test_item = filtered%test()
            result_ = assert_equals(EXAMPLE_DESCRIPTION, test_item%description())
        class default
            result_ = fail("Expected to get filter_item_result_input_t")
        end select
    end function

    function check_collection_not_matching(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(filter_item_result_t) :: filtered

        select type (input)
        type is (filter_item_result_input_t)
            filtered = input%input()
            result_ = assert_not(filtered%matched())
        class default
            result_ = fail("Expected to get filter_item_result_input_t")
        end select
    end function

    function check_collection_is_same(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_item_t) :: example_collection
        type(filter_item_result_t) :: filtered
        type(test_item_t) :: test_item

        select type (input)
        type is (filter_item_result_input_t)
            filtered = input%input()
            example_collection = example_passing_collection()
            test_item = filtered%test()
            result_ = assert_equals(example_collection%description(), test_item%description())
        class default
            result_ = fail("Expected to get filter_item_result_input_t")
        end select
    end function

    function check_collection_single_case(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(filter_item_result_t) :: filtered
        type(test_item_t) :: test_item

        select type (input)
        type is (filter_item_result_input_t)
            filtered = input%input()
            test_item = filtered%test()
            result_ = assert_equals(1, test_item%num_cases())
        class default
            result_ = fail("Expected to get filter_item_result_input_t")
        end select
    end function

    function constructor(input) result(filter_item_result_input)
        type(filter_item_result_t), intent(in) :: input
        type(filter_item_result_input_t) :: filter_item_result_input

        filter_item_result_input%input_ = input
    end function

    function input(self)
        class(filter_item_result_input_t), intent(in) :: self
        type(filter_item_result_t) :: input

        input = self%input_
    end function
end module
