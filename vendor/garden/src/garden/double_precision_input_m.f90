module garden_double_precision_input_m
    use garden_input_m, only: input_t

    implicit none
    private
    public :: double_precision_input_t

    type, extends(input_t) :: double_precision_input_t
        private
        double precision :: input_
    contains
        private
        procedure, public :: input
    end type

    interface double_precision_input_t
        module procedure constructor
    end interface
contains
    pure function constructor(input) result(double_precision_input)
        double precision, intent(in) :: input
        type(double_precision_input_t) :: double_precision_input

        double_precision_input%input_ = input
    end function

    pure function input(self)
        class(double_precision_input_t), intent(in) :: self
        double precision :: input

        input = self%input_
    end function
end module
