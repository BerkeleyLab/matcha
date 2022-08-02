module helpers_m
    use garden, only: &
            input_t, &
            test_item_t, &
            test_result_item_t, &
            transformed_t, &
            transformation_failure_t, &
            fail

    implicit none
    private
    public :: test_item_input_t, test_result_item_input_t, run_test

    type, extends(input_t) :: test_item_input_t
        private
        type(test_item_t) :: input_
    contains
        private
        procedure, public :: input => test_input
    end type

    interface test_item_input_t
        module procedure test_constructor
    end interface

    type, extends(input_t) :: test_result_item_input_t
        private
        type(test_result_item_t) :: input_
    contains
        private
        procedure, public :: input => result_input
    end type

    interface test_result_item_input_t
        module procedure result_constructor
    end interface
contains
    function run_test(input) result(example_result)
        class(input_t), intent(in) :: input
        type(transformed_t) :: example_result

        type(test_item_t) :: example

        select type (input)
        type is (test_item_input_t)
            example = input%input()
            example_result = transformed_t(test_result_item_input_t( &
                    example%run()))
        class default
            example_result = transformed_t(transformation_failure_t( &
                    fail("Expected to get a test_item_input_t")))
        end select
    end function

    function test_constructor(input) result(test_item_input)
        type(test_item_t), intent(in) :: input
        type(test_item_input_t) :: test_item_input

        test_item_input%input_ = input
    end function

    function test_input(self) result(input)
        class(test_item_input_t), intent(in) :: self
        type(test_item_t) :: input

        input = self%input_
    end function

    function result_constructor(input) result(test_result_item_input)
        type(test_result_item_t), intent(in) :: input
        type(test_result_item_input_t) :: test_result_item_input

        test_result_item_input%input_ = input
    end function

    function result_input(self) result(input)
        class(test_result_item_input_t), intent(in) :: self
        type(test_result_item_t) :: input

        input = self%input_
    end function
end module
