module garden_shrink_result_m
    use garden_input_m, only: input_t

    implicit none
    private
    public :: shrink_result_t, shrunk_value, simplest_value

    type :: shrink_result_t
        private
        class(input_t), allocatable :: input_
        logical :: simplest_
    contains
        private
        procedure, public :: input
        procedure, public :: simplest
    end type
contains
    function shrunk_value(input)
        class(input_t), intent(in) :: input
        type(shrink_result_t) :: shrunk_value

        allocate(shrunk_value%input_, source = input)
        shrunk_value%simplest_ = .false.
    end function

    function simplest_value(input)
        class(input_t), intent(in) :: input
        type(shrink_result_t) :: simplest_value

        allocate(simplest_value%input_, source = input)
        simplest_value%simplest_ = .true.
    end function

    pure function simplest(self)
        class(shrink_result_t), intent(in) :: self
        logical :: simplest

        simplest = self%simplest_
    end function

    function input(self)
        class(shrink_result_t), intent(in) :: self
        class(input_t), allocatable :: input

        allocate(input, source = self%input_)
    end function
end module
