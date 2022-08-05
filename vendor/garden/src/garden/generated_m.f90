module garden_generated_m
    use garden_input_m, only: input_t

    implicit none
    private
    public :: generated_t

    type :: generated_t
        private
        class(input_t), allocatable :: input_
    contains
        private
        procedure, public :: input
    end type

    interface generated_t
        module procedure constructor
    end interface
contains
    function constructor(value_) result(generated)
        class(input_t), intent(in) :: value_
        type(generated_t) :: generated

        allocate(generated%input_, source = value_)
    end function

    function input(self)
        class(generated_t), intent(in) :: self
        class(input_t), allocatable :: input

        allocate(input, source = self%input_)
    end function
end module
