module string_and_integer_input_m
    use iso_varying_string, only: varying_string
    use veggies, only: input_t

    implicit none
    private
    public :: string_and_integer_input_t

    type, extends(input_t) :: string_and_integer_input_t
        private
        type(varying_string) :: string_
        integer :: integer__
    contains
        private
        procedure, public :: string
        procedure, public :: integer_
    end type

    interface string_and_integer_input_t
        module procedure constructor
    end interface
contains
    pure function constructor(string, integer_) result(string_and_integer_input)
        type(varying_string), intent(in) :: string
        integer, intent(in) :: integer_
        type(string_and_integer_input_t) :: string_and_integer_input

        string_and_integer_input%string_ = string
        string_and_integer_input%integer__ = integer_
    end function

    pure function string(self)
        class(string_and_integer_input_t), intent(in) :: self
        type(varying_string) :: string

        string = self%string_
    end function

    pure function integer_(self)
        class(string_and_integer_input_t), intent(in) :: self
        integer :: integer_

        integer_ = self%integer__
    end function
end module
