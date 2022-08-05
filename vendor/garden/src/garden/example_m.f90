module garden_example_m
    use garden_input_m, only: input_t

    implicit none
    private
    public :: example_t

    type :: example_t
        private
        class(input_t), allocatable :: input_
    contains
        private
        procedure, public :: input
    end type

    interface example_t
        module procedure constructor
    end interface
contains
    function constructor(input) result(example)
        class(input_t), intent(in) :: input
        type(example_t) :: example

        allocate(example%input_, source = input)
    end function

    function input(self)
        class(example_t), intent(in) :: self
        class(input_t), allocatable :: input

        allocate(input, source = self%input_)
    end function
end module
