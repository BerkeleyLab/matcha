module ascii_string_pair_generator_m
    use iso_varying_string, only: varying_string, extract, len, var_str
    use string_pair_input_m, only: string_pair_input_t
    use veggies, only: &
            generated_t, &
            generator_t, &
            input_t, &
            shrink_result_t, &
            get_random_ascii_string, &
            shrunk_value, &
            simplest_value

    implicit none
    private
    public :: ascii_string_pair_generator_t, ASCII_STRING_PAIR_GENERATOR

    type, extends(generator_t) :: ascii_string_pair_generator_t
    contains
        private
        procedure, public :: generate
        procedure, public, nopass :: shrink
    end type

    type(ascii_string_pair_generator_t), parameter ::  &
            ASCII_STRING_PAIR_GENERATOR = ascii_string_pair_generator_t()
contains
    function generate(self) result(generated_value)
        class(ascii_string_pair_generator_t), intent(in) :: self
        type(generated_t) :: generated_value

        associate(unused => self)
        end associate

        generated_value = generated_t(string_pair_input_t( &
                get_random_ascii_string(), get_random_ascii_string()))
    end function

    function shrink(input) result(shrunk)
        class(input_t), intent(in) :: input
        type(shrink_result_t) :: shrunk

        type(varying_string) :: first, second

        select type (input)
        type is (string_pair_input_t)
            first = input%first()
            second = input%second()
            if (len(first) <= 1) then
                if (len(second) <= 1) then
                    shrunk = simplest_value(string_pair_input_t( &
                            var_str(""), var_str("")))
                else
                    shrunk = shrunk_value(string_pair_input_t( &
                            var_str(""), extract(second, finish=len(second)-1)))
                end if
            else
                if (len(second) <= 1) then
                    shrunk = shrunk_value(string_pair_input_t( &
                            extract(first, finish=len(first)-1), var_str("")))
                else
                    shrunk = shrunk_value(string_pair_input_t( &
                            extract(first, finish=len(first)-1), &
                            extract(second, finish=len(second)-1)))
                end if
            end if
        end select
    end function
end module
