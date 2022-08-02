module collection_properties_test
    use example_collections_m, only: &
            example_passing_collection, &
            example_test_case_1, &
            example_test_case_2, &
            EXAMPLE_CASE_DESCRIPTION_1, &
            EXAMPLE_CASE_DESCRIPTION_2, &
            EXAMPLE_COLLECTION_DESCRIPTION, &
            NUM_CASES_IN_PASSING
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
    public :: test_collection_properties
contains
    function test_collection_properties() result(test)
        type(test_item_t) :: test

        test = describe( &
                "A test collection", &
                test_item_input_t(example_passing_collection()), &
                [ it_("can tell how many tests it has", check_num_cases) &
                , it_( &
                        "includes the given description", &
                        check_collection_top_description) &
                , it_( &
                        "includes the individual test descriptions", &
                        check_collection_descriptions) &
                , it_( &
                        "takes less than three times as long as the individual cases", &
                        check_speed) &
                ])
    end function

    function check_num_cases(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_item_t) :: example_collection

        select type (input)
        class is (test_item_input_t)
            example_collection = input%input()
            result_ = assert_equals(NUM_CASES_IN_PASSING, example_collection%num_cases())
        class default
            result_ = fail("Expected to get a test_item_input_t")
        end select
    end function

    function check_collection_top_description(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_item_t) :: example_collection

        select type (input)
        class is (test_item_input_t)
            example_collection = input%input()
            result_ = assert_includes( &
                    EXAMPLE_COLLECTION_DESCRIPTION, example_collection%description())
        class default
            result_ = fail("Expected to get a test_item_input_t")
        end select
    end function

    function check_collection_descriptions(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_item_t) :: example_collection

        select type (input)
        class is (test_item_input_t)
            example_collection = input%input()
            result_ = &
                    assert_includes( &
                            EXAMPLE_CASE_DESCRIPTION_1, example_collection%description()) &
                    .and.assert_includes( &
                            EXAMPLE_CASE_DESCRIPTION_2, example_collection%description())
        class default
            result_ = fail("Expected to get a test_item_input_t")
        end select
    end function

    function check_speed(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(test_item_t) :: internal_collection

        type(test_item_t) :: the_cases(3)

        the_cases(1) = example_test_case_1()
        the_cases(2) = example_test_case_2()
        the_cases(3) = example_test_case_2()

        select type (input)
        type is (test_item_input_t)
            internal_collection = input%input()
            result_ = assert_faster_than(runCases, runCollection, 100)
        class default
            result_ = fail("Expected to get a test_item_input_t")
        end select
    contains
        subroutine runCollection
            integer :: i
            type(test_result_item_t) :: internal_result

            do i = 1, 100
                internal_result = internal_collection%run()
            end do
        end subroutine

        subroutine runCases
            integer :: i
            type(test_result_item_t) :: the_results(3)

            do i = 1, 300
                the_results(1) = the_cases(1)%run()
                the_results(2) = the_cases(2)%run()
                the_results(3) = the_cases(3)%run()
            end do
        end subroutine
    end function
end module
