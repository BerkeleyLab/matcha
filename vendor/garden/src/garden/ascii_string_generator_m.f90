module garden_ascii_string_generator_m
    use iso_varying_string, only: assignment(=), extract, len, var_str
    use garden_generated_m, only: generated_t
    use garden_generator_m, only: generator_t
    use garden_input_m, only: input_t
    use garden_shrink_result_m, only: &
            shrink_result_t, simplest_value, shrunk_value
    use garden_string_input_m, only: string_input_t
    use garden_random_m, only: get_random_ascii_string
    use garden_string_input_m, only: string_input_t

    implicit none
    private
    public :: ASCII_STRING_GENERATOR

    type, extends(generator_t) :: ascii_string_generator_t
    contains
        private
        procedure, public :: generate
        procedure, nopass, public :: shrink
    end type

    type(ascii_string_generator_t), parameter :: &
            ASCII_STRING_GENERATOR = ascii_string_generator_t()
contains
    function generate(self) result(generated_value)
        class(ascii_string_generator_t), intent(in) :: self
        type(generated_t) :: generated_value

        associate(unused => self)
        end associate

        generated_value = generated_t(string_input_t(get_random_ascii_string()))
    end function

    function shrink(input) result(shrunk)
        class(input_t), intent(in) :: input
        type(shrink_result_t) :: shrunk

        select type (input)
        type is (string_input_t)
            if (len(input%input()) <= 1) then
                shrunk = simplest_value(string_input_t(var_str("")))
            else
                shrunk = shrunk_value(string_input_t(extract( &
                        input%input(), 1, len(input%input()) - 1)))
            end if
        end select
    end function
end module
