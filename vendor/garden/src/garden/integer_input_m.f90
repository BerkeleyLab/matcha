module garden_integer_input_m
    use garden_input_m, only: input_t

    implicit none
    private
    public :: integer_input_t

    type, extends(input_t) :: integer_input_t
        private
        integer :: input_
    contains
        private
        procedure, public :: input
    end type

    interface integer_input_t
        module procedure constructor
    end interface
contains
    pure function constructor(input) result(integer_input)
        integer, intent(in) :: input
        type(integer_input_t) :: integer_input

        integer_input%input_ = input
    end function

    pure function input(self)
        class(integer_input_t), intent(in) :: self
        integer :: input

        input = self%input_
    end function
end module
