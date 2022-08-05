module garden_test_result_item_m
    use iso_varying_string, only: varying_string
    use garden_test_result_m, only: test_result_t

    implicit none
    private
    public :: test_result_item_t

    type :: test_result_item_t
        private
        class(test_result_t), allocatable :: result_
    contains
        private
        procedure, public :: num_asserts
        procedure, public :: num_cases
        procedure, public :: num_failing_asserts
        procedure, public :: num_failing_cases
        procedure, public :: passed
        procedure, public :: failure_description
        procedure, public :: verbose_description
    end type

    interface test_result_item_t
        module procedure constructor
    end interface
contains
    function constructor(result_) result(test_result_item)
        class(test_result_t), intent(in) :: result_
        type(test_result_item_t) :: test_result_item

        allocate(test_result_item%result_, source = result_)
    end function

    pure recursive function failure_description( &
            self, colorize) result(description)
        class(test_result_item_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(varying_string) :: description

        description = self%result_%failure_description(colorize)
    end function

    pure recursive function num_asserts(self)
        class(test_result_item_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = self%result_%num_asserts()
    end function

    pure recursive function num_cases(self)
        class(test_result_item_t), intent(in) :: self
        integer :: num_cases

        num_cases = self%result_%num_cases()
    end function

    pure recursive function num_failing_asserts(self) result(num_asserts)
        class(test_result_item_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = self%result_%num_failing_asserts()
    end function

    pure recursive function num_failing_cases(self) result(num_cases)
        class(test_result_item_t), intent(in) :: self
        integer :: num_cases

        num_cases = self%result_%num_failing_cases()
    end function

    pure recursive function passed(self)
        class(test_result_item_t), intent(in) :: self
        logical :: passed

        passed = self%result_%passed()
    end function

    pure recursive function verbose_description( &
            self, colorize) result(description)
        class(test_result_item_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(varying_string) :: description

        description = self%result_%verbose_description(colorize)
    end function
end module
