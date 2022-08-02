module garden_utilities_m
    use iso_varying_string, only: varying_string, operator(//)
    use strff, only: add_hanging_indentation, join, strff_to_string => to_string, NEWLINE

    implicit none
    private
    public :: to_string, equals_within_absolute, equals_within_relative

    interface to_string
        module procedure double_array_to_string
        module procedure double_matrix_to_string
        module procedure double_tensor_to_string
        module procedure integer_array_to_string
        module procedure integer_matrix_to_string
        module procedure integer_tensor_to_string
    end interface
contains
    pure function double_array_to_string(array) result(string)
        double precision, intent(in) :: array(:)
        type(varying_string) :: string

        string = "[" // join(strff_to_string(array), ", ") // "]"
    end function

    pure function double_matrix_to_string(matrix) result(string)
        double precision, intent(in) :: matrix(:,:)
        type(varying_string) :: string

        integer :: i

        string = add_hanging_indentation( &
            "[" // join([(to_string(matrix(i,:)), i = 1, size(matrix, dim=1))], "," // NEWLINE) // "]", &
            1)
    end function

    pure function double_tensor_to_string(tensor) result(string)
        double precision, intent(in) :: tensor(:,:,:)
        type(varying_string) :: string

        integer :: i

        string = add_hanging_indentation( &
            "[" // join([(to_string(tensor(i,:,:)), i = 1, size(tensor, dim=1))], "," // NEWLINE) // "]", &
            1)
    end function

    pure function integer_array_to_string(array) result(string)
        integer, intent(in) :: array(:)
        type(varying_string) :: string

        string = "[" // join(strff_to_string(array), ", ") // "]"
    end function

    pure function integer_matrix_to_string(matrix) result(string)
        integer, intent(in) :: matrix(:,:)
        type(varying_string) :: string

        integer :: i

        string = add_hanging_indentation( &
            "[" // join([(to_string(matrix(i,:)), i = 1, size(matrix, dim=1))], "," // NEWLINE) // "]", &
            1)
    end function

    pure function integer_tensor_to_string(tensor) result(string)
        integer, intent(in) :: tensor(:,:,:)
        type(varying_string) :: string

        integer :: i

        string = add_hanging_indentation( &
            "[" // join([(to_string(tensor(i,:,:)), i = 1, size(tensor, dim=1))], "," // NEWLINE) // "]", &
            1)
    end function

    elemental function equals_within_absolute(expected, actual, tolerance)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        logical :: equals_within_absolute

        equals_within_absolute = abs(expected - actual) <= tolerance
    end function

    elemental function equals_within_relative(expected, actual, tolerance)
        double precision, intent(in) :: expected
        double precision, intent(in) :: actual
        double precision, intent(in) :: tolerance
        logical :: equals_within_relative

        double precision, parameter :: MACHINE_TINY = tiny(0.0d0)

        equals_within_relative = &
                (abs(expected) <= MACHINE_TINY .and. abs(actual) <= MACHINE_TINY) &
                .or. (abs(expected - actual) / abs(expected) <= tolerance)
    end function
end module
