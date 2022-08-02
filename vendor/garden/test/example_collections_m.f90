module example_collections_m
    use example_asserts_m, only: &
            example_multiple_asserts, &
            NUM_PASSING_FROM_EXAMPLE => NUM_ASSERTS_IN_PASSING
    use garden, only: result_t, test_item_t, describe, fail, it

    implicit none
    private
    public :: &
            example_failing_collection, &
            example_passing_collection, &
            example_test_case_1, &
            example_test_case_2, &
            EXAMPLE_CASE_DESCRIPTION_1, &
            EXAMPLE_CASE_DESCRIPTION_2, &
            EXAMPLE_COLLECTION_DESCRIPTION, &
            EXAMPLE_FAILING_CASE_DESCRIPTION, &
            FAILURE_MESSAGE, &
            MIDDLE_COLLECTION_DESCRIPTION, &
            NOT_IN_DESCRIPTIONS, &
            NUM_ASSERTS_IN_FAILING, &
            NUM_ASSERTS_IN_PASSING, &
            NUM_CASES_IN_FAILING, &
            NUM_CASES_IN_PASSING, &
            NUM_FAILING_ASSERTS, &
            NUM_FAILING_CASES

    character(len=*), parameter :: EXAMPLE_CASE_DESCRIPTION_1 = &
            "Example Case Description 1"
    character(len=*), parameter :: EXAMPLE_CASE_DESCRIPTION_2 = &
            "Example Case Description 2"
    character(len=*), parameter :: EXAMPLE_COLLECTION_DESCRIPTION = &
            "Example Collection Description"
    character(len=*), parameter :: EXAMPLE_FAILING_CASE_DESCRIPTION = &
            "Example Failing Case Description"
    character(len=*), parameter :: FAILURE_MESSAGE = "Failure Message"
    character(len=*), parameter :: MIDDLE_COLLECTION_DESCRIPTION = &
            "Middle Collection Description"
    character(len=*), parameter :: NOT_IN_DESCRIPTIONS = "NOT IN DESCRIPTION"
    integer, parameter :: NUM_ASSERTS_IN_FAILING = NUM_PASSING_FROM_EXAMPLE * 2 + 1
    integer, parameter :: NUM_ASSERTS_IN_PASSING = NUM_PASSING_FROM_EXAMPLE * 3
    integer, parameter :: NUM_CASES_IN_FAILING = 3
    integer, parameter :: NUM_CASES_IN_PASSING = 3
    integer, parameter :: NUM_FAILING_ASSERTS = 1
    integer, parameter :: NUM_FAILING_CASES = 1
contains
    function example_test_case_1() result(test_case)
        type(test_item_t) :: test_case

        test_case = it(EXAMPLE_CASE_DESCRIPTION_1, example_multiple_asserts)
    end function

    function example_test_case_2() result(test_case)
        type(test_item_t) :: test_case

        test_case = it(EXAMPLE_CASE_DESCRIPTION_2, example_multiple_asserts)
    end function

    pure function example_fail() result(result_)
        type(result_t) :: result_

        result_ = fail(FAILURE_MESSAGE)
    end function

    function example_failing_test_case() result(test_case)
        type(test_item_t) :: test_case

        test_case = it(EXAMPLE_FAILING_CASE_DESCRIPTION, example_fail)
    end function

    function example_failing_collection() result(test_collection)
        type(test_item_t) :: test_collection

        type(test_item_t) :: cases(3)

        cases(1) = example_test_case_1()
        cases(2) = example_test_case_2()
        cases(3) = example_failing_test_case()
        test_collection = describe(EXAMPLE_COLLECTION_DESCRIPTION, cases)
    end function

    function middle_collection() result(test_collection)
        type(test_item_t) :: test_collection

        type(test_item_t) :: cases(2)

        cases(1) = example_test_case_1()
        cases(2) = example_test_case_2()
        test_collection = describe(MIDDLE_COLLECTION_DESCRIPTION, cases)
    end function

    function example_passing_collection() result(test_collection)
        type(test_item_t) :: test_collection

        type(test_item_t) :: items(2)

        items(1) = middle_collection()
        items(2) = example_test_case_2()
        test_collection = describe(EXAMPLE_COLLECTION_DESCRIPTION, items)
    end function
end module
