module garden_test_interfaces_m
    use garden_input_m, only: input_t
    use garden_result_m, only: result_t
    use garden_transformed_m, only: transformed_t

    implicit none
    private
    public :: computation_i, input_test_i, simple_test_i, transformer_i

    abstract interface
        subroutine computation_i
        end subroutine

        function input_test_i(input) result(result_)
            import :: input_t, result_t

            implicit none

            class(input_t), intent(in) :: input
            type(result_t) :: result_
        end function

        function simple_test_i() result(result_)
            import :: result_t

            implicit none

            type(result_t) :: result_
        end function

        function transformer_i(input) result(output)
            import :: input_t, transformed_t

            implicit none

            class(input_t), intent(in) :: input
            type(transformed_t) :: output
        end function
    end interface
end module
