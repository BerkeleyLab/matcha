module garden_string_input_m
    use iso_varying_string, only: varying_string
    use garden_input_m, only: input_t

    implicit none
    private
    public :: string_input_t

    type, extends(input_t) :: string_input_t
        private
        type(varying_string) :: input_
    contains
        private
        procedure, public :: input
    end type

    interface string_input_t
        module procedure constructor
    end interface
contains
    pure function constructor(input) result(string_input)
        type(varying_string), intent(in) :: input
        type(string_input_t) :: string_input

        string_input%input_ = input
    end function

    pure function input(self)
        class(string_input_t), intent(in) :: self
        type(varying_string) :: input

        input = self%input_
    end function
end module
