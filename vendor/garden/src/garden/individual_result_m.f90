module garden_individual_result_m
    use iso_varying_string, only: varying_string, assignment(=), operator(//)

    implicit none
    private
    public :: individual_result_t

    type :: individual_result_t
        private
        type(varying_string) :: message
        logical :: passed_
    contains
        private
        procedure, public :: passed
        procedure, public :: failure_description
        procedure, public :: verbose_description
    end type

    interface individual_result_t
        module procedure constructor
    end interface
contains
    pure function constructor(message, passed) result(individual_result)
        type(varying_string), intent(in) :: message
        logical, intent(in) :: passed
        type(individual_result_t) :: individual_result

        individual_result%message = message
        individual_result%passed_ = passed
    end function

    elemental function failure_description(self, colorize) result(description)
        class(individual_result_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(varying_string) :: description

        if (self%passed_) then
            description = ""
        else
            if (colorize) then
                description = char(27) // "[31m" // self%message // char(27) // "[0m"
            else
                description = self%message
            end if
        end if
    end function

    elemental function passed(self)
        class(individual_result_t), intent(in) :: self
        logical :: passed

        passed = self%passed_
    end function

    elemental function verbose_description(self, colorize) result(description)
        class(individual_result_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(varying_string) :: description

        if (colorize) then
            if (self%passed_) then
                description = char(27) // "[32m" // self%message // char(27) // "[0m"
            else
                description = char(27) // "[31m" // self%message // char(27) // "[0m"
            end if
        else
            description = self%message
        end if
    end function
end module
