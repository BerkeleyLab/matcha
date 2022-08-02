module garden_test_item_m
    use iso_varying_string, only: varying_string
    use garden_input_m, only: input_t
    use garden_test_m, only: filter_result_t, test_t
    use garden_test_result_item_m, only: test_result_item_t

    implicit none
    private
    public :: filter_item_result_t, test_item_t

    type :: test_item_t
        private
        class(test_t), allocatable :: test
    contains
        private
        procedure, public :: description
        procedure, public :: filter
        procedure, public :: num_cases
        procedure :: run_with_input
        procedure :: run_without_input
        generic, public :: run => run_with_input, run_without_input
    end type

    type :: filter_item_result_t
        private
        type(test_item_t) :: test_
        logical :: matched_
    contains
        private
        procedure, public :: matched
        procedure, public :: test
    end type

    interface test_item_t
        module procedure constructor
    end interface
contains
    function constructor(test) result(test_item)
        class(test_t), intent(in) :: test
        type(test_item_t) :: test_item

        allocate(test_item%test, source = test)
    end function

    pure recursive function description(self)
        class(test_item_t), intent(in) :: self
        type(varying_string) :: description

        description = self%test%description()
    end function

    recursive function filter(self, filter_string) result(filter_result)
        class(test_item_t), intent(in) :: self
        type(varying_string), intent(in) :: filter_string
        type(filter_item_result_t) :: filter_result

        type(filter_result_t) :: result_

        result_ = self%test%filter(filter_string)
        if (result_%matched()) then
            filter_result = filter_matched(result_%test())
        else
            filter_result = filter_failed()
        end if
    end function

    pure recursive function num_cases(self)
        class(test_item_t), intent(in) :: self
        integer :: num_cases

        num_cases = self%test%num_cases()
    end function

    recursive function run_with_input(self, input) result(result_)
        class(test_item_t), intent(in) :: self
        class(input_t), intent(in) :: input
        type(test_result_item_t) :: result_

        result_ = self%test%run(input)
    end function

    recursive function run_without_input(self) result(result_)
        class(test_item_t), intent(in) :: self
        type(test_result_item_t) :: result_

        result_ = self%test%run()
    end function

    function filter_failed()
        type(filter_item_result_t) :: filter_failed

        filter_failed%matched_ = .false.
    end function

    function filter_matched(test)
        class(test_t), intent(in) :: test
        type(filter_item_result_t) :: filter_matched

        filter_matched%test_ = test_item_t(test)
        filter_matched%matched_ = .true.
    end function

    elemental function matched(self)
        class(filter_item_result_t), intent(in) :: self
        logical :: matched

        matched = self%matched_
    end function

    impure elemental function test(self)
        class(filter_item_result_t), intent(in) :: self
        type(test_item_t) :: test

        test = self%test_
    end function
end module
