module garden_transformed_m
    use garden_input_m, only: input_t

    implicit none
    private
    public :: transformed_t

    type :: transformed_t
        private
        class(input_t), allocatable :: input_
    contains
        private
        procedure, public :: input
    end type

    interface transformed_t
        module procedure constructor
    end interface
contains
    function constructor(input) result(transformed)
        class(input_t), intent(in) :: input
        type(transformed_t) :: transformed

        allocate(transformed%input_, source = input)
    end function

    function input(self)
        class(transformed_t), intent(in) :: self
        class(input_t), allocatable :: input

        allocate(input, source = self%input_)
    end function
end module
