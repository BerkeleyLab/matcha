module garden_random_m
    use iso_varying_string, only: varying_string, assignment(=)

    implicit none
    private
    public :: &
            get_random_ascii_character, &
            get_random_ascii_string, &
            get_random_ascii_string_with_max_length, &
            get_random_double_precision_with_magnitude, &
            get_random_double_precision_with_range, &
            get_random_integer, &
            get_random_integer_with_range, &
            get_random_logical
contains
    function get_random_ascii_character() result(random_character)
        character(len=1) :: random_character

        character(len=*), parameter :: ASCII_CHARACTERS = &
        '  !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~'
        integer :: which_character

        which_character = get_random_integer_with_range(1, len(ASCII_CHARACTERS))
        random_character = ASCII_CHARACTERS(which_character:which_character)
    end function

    function get_random_ascii_string() result(random_string)
        type(varying_string) :: random_string

        random_string = get_random_ascii_string_with_max_length(1024)
    end function

    function get_random_ascii_string_with_max_length(max_length) result(random_string)
        integer, intent(in) :: max_length
        type(varying_string) :: random_string

        character(len=max_length) :: characters
        integer :: i
        integer :: num_characters

        num_characters = get_random_integer_with_range(0, max_length)
        do i = 1, num_characters
            characters(i:i) = get_random_ascii_character()
        end do
        random_string = characters(1:num_characters)
    end function

    function get_random_double_precision_with_magnitude(magnitude) result(random_double)
        double precision, intent(in) :: magnitude
        double precision :: random_double

        call random_number(random_double)
        random_double = random_double * magnitude
        if (get_random_logical()) random_double = -random_double
    end function

    function get_random_double_precision_with_range(start, end_) result(random_double)
        double precision, intent(in) :: start
        double precision, intent(in) :: end_
        double precision :: random_double

        call random_number(random_double)
        random_double = start + (end_ - start) * random_double
    end function

    function get_random_integer() result(random_integer)
        integer :: random_integer

        integer, parameter :: MAX_INT = HUGE(1)
        double precision :: random_real

        call random_number(random_real)
        random_integer = floor(random_real*MAX_INT)
        if (get_random_logical()) random_integer = -random_integer
    end function

    function get_random_integer_with_range(start, end_) result(random_integer)
        integer, intent(in) :: start
        integer, intent(in) :: end_
        integer :: random_integer

        double precision :: random_real

        call random_number(random_real)
        random_integer = start + floor((end_ + 1 - start) * random_real)
    end function

    function get_random_logical() result(random_logical)
        logical :: random_logical

        if (get_random_integer_with_range(0, 1) == 0) then
            random_logical = .TRUE.
        else
            random_logical = .FALSE.
        end if
    end function
end module
