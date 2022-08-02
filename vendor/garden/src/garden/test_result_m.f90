module garden_test_result_m
    use iso_varying_string, only: varying_string

    implicit none
    private
    public :: test_result_t

    type, abstract :: test_result_t
    contains
        private
        procedure(test_result_count_i), public, deferred :: num_asserts
        procedure(test_result_count_i), public, deferred :: num_cases
        procedure(test_result_count_i), public, deferred :: num_failing_asserts
        procedure(test_result_count_i), public, deferred :: num_failing_cases
        procedure(test_result_passed_i), public, deferred :: passed
        procedure(test_result_colorized_description_i), public, deferred :: &
                failure_description
        procedure(test_result_colorized_description_i), public, deferred :: &
                verbose_description
    end type

    abstract interface
        pure function test_result_colorized_description_i( &
                self, colorize) result(description)
            import :: test_result_t, varying_string

            implicit none

            class(test_result_t), intent(in) :: self
            logical, intent(in) :: colorize
            type(varying_string) :: description
        end function

        pure function test_result_count_i(self) result(num)
            import :: test_result_t

            implicit none

            class(test_result_t), intent(in) :: self
            integer :: num
        end function

        pure function test_result_passed_i(self) result(passed)
            import :: test_result_t

            implicit none

            class(test_result_t), intent(in) :: self
            logical :: passed
        end function
    end interface
end module
