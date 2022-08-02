module garden_test_case_result_m
    use iso_varying_string, only: varying_string, assignment(=), operator(//)
    use strff, only: add_hanging_indentation, NEWLINE
    use garden_constants_m, only: INDENTATION
    use garden_result_m, only: result_t
    use garden_test_result_m, only: test_result_t

    implicit none
    private
    public :: test_case_result_t

    type, extends(test_result_t) :: test_case_result_t
        private
        type(varying_string) :: description
        type(result_t) :: result_
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

    interface test_case_result_t
        module procedure constructor
    end interface
contains
    pure function constructor(description, result_) result(test_case_result)
        type(varying_string), intent(in) :: description
        type(result_t), intent(in) :: result_
        type(test_case_result_t) :: test_case_result

        test_case_result%description = description
        test_case_result%result_ = result_
    end function

    pure function failure_description( &
            self, colorize) result(description)
        class(test_case_result_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(varying_string) :: description

        if (self%passed()) then
            description = ""
        else
            description = add_hanging_indentation( &
                    self%description // NEWLINE &
                        // self%result_%failure_description(colorize), &
                    INDENTATION)
        end if
    end function

    pure function num_asserts(self)
        class(test_case_result_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = self%result_%num_asserts()
    end function

    pure function num_cases(self)
        class(test_case_result_t), intent(in) :: self
        integer :: num_cases

        associate(unused => self)
        end associate

        num_cases = 1
    end function

    pure function num_failing_asserts(self) result(num_asserts)
        class(test_case_result_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = self%result_%num_failing_asserts()
    end function

    pure function num_failing_cases(self) result(num_cases)
        class(test_case_result_t), intent(in) :: self
        integer :: num_cases

        if (self%passed()) then
            num_cases = 0
        else
            num_cases = 1
        end if
    end function

    pure function passed(self)
        class(test_case_result_t), intent(in) :: self
        logical :: passed

        passed = self%result_%passed()
    end function

    pure function verbose_description( &
            self, colorize) result(description)
        class(test_case_result_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(varying_string) :: description

        description = add_hanging_indentation( &
                self%description // NEWLINE &
                    // self%result_%verbose_description(colorize), &
                INDENTATION)
    end function
end module
