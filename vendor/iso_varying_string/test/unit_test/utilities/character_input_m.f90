module character_input_m
    use veggies, only: input_t

    implicit none
    private
    public :: character_input_t

    type, extends(input_t) :: character_input_t
        private
        character(len=1) :: input_
    contains
        private
        procedure, public :: input
    end type

    interface character_input_t
        module procedure constructor
    end interface
contains
    pure function constructor(input) result(character_input)
        character(len=1), intent(in) :: input
        type(character_input_t) :: character_input

        character_input%input_ = input
    end function

    pure function input(self)
        class(character_input_t), intent(in) :: self
        character(len=1) :: input

        input = self%input_
    end function
end module
