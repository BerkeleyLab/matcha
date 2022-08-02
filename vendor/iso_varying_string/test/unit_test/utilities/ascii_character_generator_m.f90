module ascii_character_generator_m
    use character_input_m, only: character_input_t
    use veggies, only: &
            generated_t, &
            generator_t, &
            input_t, &
            shrink_result_t, &
            get_random_ascii_character, &
            simplest_value

    implicit none
    private
    public :: ascii_character_generator_t, ASCII_CHARACTER_GENERATOR

    type, extends(generator_t) :: ascii_character_generator_t
    contains
        private
        procedure, public :: generate
        procedure, public, nopass :: shrink
    end type

    type(ascii_character_generator_t), parameter :: &
            ASCII_CHARACTER_GENERATOR = ascii_character_generator_t()
contains
    function generate(self) result(generated_value)
        class(ascii_character_generator_t), intent(in) :: self
        type(generated_t) :: generated_value

        associate(unused => self)
        end associate

        generated_value = generated_t(character_input_t(get_random_ascii_character()))
    end function

    function shrink(input) result(shrunk)
        class(input_t), intent(in) :: input
        type(shrink_result_t) :: shrunk

        shrunk = simplest_value(input)
    end function
end module
