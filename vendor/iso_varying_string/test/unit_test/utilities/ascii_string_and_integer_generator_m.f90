module ascii_string_and_integer_generator_m
    use iso_varying_string, only: varying_string, extract, len, var_str
    use string_and_integer_input_m, only: string_and_integer_input_t
    use veggies, only: &
            generated_t, &
            generator_t, &
            input_t, &
            shrink_result_t, &
            get_random_integer_with_range, &
            get_random_ascii_string, &
            shrunk_value, &
            simplest_value
    implicit none
    private
    public :: &
            ascii_string_and_integer_generator_t, &
            ASCII_STRING_AND_INTEGER_GENERATOR

    type, extends(generator_t) :: ascii_string_and_integer_generator_t
    contains
        private
        procedure, public :: generate
        procedure, public, nopass :: shrink
    end type

    type(ascii_string_and_integer_generator_t), parameter :: &
            ASCII_STRING_AND_INTEGER_GENERATOR = ascii_string_and_integer_generator_t()
contains
    function generate(self) result(generated_value)
        class(ascii_string_and_integer_generator_t), intent(in) :: self
        type(generated_t) :: generated_value

        associate(unused => self)
        end associate

        generated_value = generated_t(string_and_integer_input_t( &
                get_random_ascii_string(), get_random_integer_with_range(0, 10)))
    end function

    function shrink(input) result(shrunk)
        class(input_t), intent(in) :: input
        type(shrink_result_t) :: shrunk

        integer :: integer_
        type(varying_string) :: string

        select type (input)
        type is (string_and_integer_input_t)
            integer_ = input%integer_()
            string = input%string()
            if (integer_ == 0) then
                if (len(string) <= 1) then
                    shrunk = simplest_value(string_and_integer_input_t( &
                            var_str(""), 0))
                else
                    shrunk = shrunk_value(string_and_integer_input_t( &
                            extract(string, finish=len(string)-1), 0))
                end if
            else
                if (len(string) <= 1) then
                    shrunk = shrunk_value(string_and_integer_input_t( &
                            var_str(""), integer_/2))
                else
                    shrunk = shrunk_value(string_and_integer_input_t( &
                            extract(string, finish=len(string)-1), integer_/2))
                end if
            end if
        end select
    end function
end module
