module string_pair_input_m
    use iso_varying_string, only: varying_string
    use veggies, only: input_t

    implicit none
    private
    public :: string_pair_input_t

    type, extends(input_t) :: string_pair_input_t
        private
        type(varying_string) :: first_
        type(varying_string) :: second_
    contains
        private
        procedure, public :: first
        procedure, public :: second
    end type

    interface string_pair_input_t
        module procedure constructor
    end interface
contains
    pure function constructor(first, second) result(string_pair_input)
        type(varying_string), intent(in) :: first
        type(varying_string), intent(in) :: second
        type(string_pair_input_t) :: string_pair_input

        string_pair_input%first_ = first
        string_pair_input%second_ = second
    end function

    pure function first(self)
        class(string_pair_input_t), intent(in) :: self
        type(varying_string) :: first

        first = self%first_
    end function

    pure function second(self)
        class(string_pair_input_t), intent(in) :: self
        type(varying_string) :: second

        second = self%second_
    end function
end module
