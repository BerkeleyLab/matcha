module iso_varying_string
    implicit none
    private
    public :: &
            varying_string, &
            assignment(=), &
            operator(//), &
            operator(==), &
            operator(/=), &
            operator(<), &
            operator(<=), &
            operator(>), &
            operator(>=), &
            adjustl, &
            adjustr, &
            char, &
            iachar, &
            ichar, &
            index, &
            len, &
            len_trim, &
            lge, &
            lgt, &
            lle, &
            llt, &
            repeat, &
            scan, &
            trim, &
            verify, &
            var_str, &
            get, &
            put, &
            put_line, &
            extract, &
            insert, &
            remove, &
            replace, &
            split

    type :: varying_string ! Sec. 3.2
        private
        character(len=1), allocatable :: characters(:)
    end type

    interface assignment(=) ! Sec. 3.3.1
        module procedure assign_character_to_string
        module procedure assign_string_to_character
    end interface

    interface operator(//) ! Sec. 3.3.2
        module procedure concat_strings
        module procedure concat_string_and_character
        module procedure concat_character_and_string
    end interface

    interface operator(==) ! Sec. 3.3.3
        module procedure string_eq_string
        module procedure character_eq_string
        module procedure string_eq_character
    end interface

    interface operator(/=) ! Sec. 3.3.3
        module procedure string_ne_string
        module procedure character_ne_string
        module procedure string_ne_character
    end interface

    interface operator(<) ! Sec. 3.3.3
        module procedure string_lt_string
        module procedure character_lt_string
        module procedure string_lt_character
    end interface

    interface operator(<=) ! Sec. 3.3.3
        module procedure string_le_string
        module procedure character_le_string
        module procedure string_le_character
    end interface

    interface operator(>) ! Sec. 3.3.3
        module procedure string_gt_string
        module procedure character_gt_string
        module procedure string_gt_character
    end interface

    interface operator(>=) ! Sec. 3.3.3
        module procedure string_ge_string
        module procedure character_ge_string
        module procedure string_ge_character
    end interface

    interface adjustl ! Sec. 3.4.1
        module procedure string_adjustl
    end interface

    interface adjustr ! Sec. 3.4.1
        module procedure string_adjustr
    end interface

    interface char ! Sec. 3.4.3
        module procedure string_to_char
        module procedure string_to_char_with_length
    end interface

    interface iachar ! Sec. 3.4.4
        module procedure string_iachar
    end interface

    interface ichar ! Sec. 3.4.5
        module procedure string_ichar
    end interface

    interface index ! Sec. 3.4.6
        module procedure string_index_string
        module procedure string_index_character
        module procedure character_index_string
    end interface

    interface len ! Sec. 3.4.7
        module procedure len_string
    end interface

    interface len_trim ! Sec. 3.4.8
        module procedure len_trim_string
    end interface

    interface lge ! Sec. 3.4.9
        module procedure string_lge_string
        module procedure character_lge_string
        module procedure string_lge_character
    end interface

    interface lgt ! Sec. 3.4.10
        module procedure string_lgt_string
        module procedure character_lgt_string
        module procedure string_lgt_character
    end interface

    interface lle ! Sec. 3.4.11
        module procedure string_lle_string
        module procedure character_lle_string
        module procedure string_lle_character
    end interface

    interface llt ! Sec. 3.4.12
        module procedure string_llt_string
        module procedure character_llt_string
        module procedure string_llt_character
    end interface llt

    interface repeat ! Sec. 3.4.13
        module procedure string_repeat
    end interface

    interface scan ! Sec. 3.4.14
        module procedure string_scan_string
        module procedure string_scan_character
        module procedure character_scan_string
    end interface

    interface trim ! Sec. 3.4.15
        module procedure trim_string
    end interface

    interface verify ! Sec. 3.4.16
        module procedure string_verify_string
        module procedure string_verify_character
        module procedure character_verify_string
    end interface

    interface get ! Sec. 3.6.1
        module procedure get_default_unit_to_end_of_record
        module procedure get_with_unit_to_end_of_record
        module procedure get_default_unit_to_terminator_string
        module procedure get_with_unit_to_terminator_string
        module procedure get_default_unit_to_terminator_characters
        module procedure get_with_unit_to_terminator_characters
    end interface

    interface put ! Sec. 3.6.2
        module procedure put_String_Default_Unit
        module procedure put_string_with_unit
        module procedure put_characters_default_unit
        module procedure put_characters_with_unit
    end interface

    interface put_line ! Sec. 3.6.3
        module procedure put_line_string_default_unit
        module procedure put_line_string_with_unit
        module procedure put_line_characters_default_unit
        module procedure put_line_characters_with_unit
    end interface

    interface extract ! Sec. 3.7.1
        module procedure extract_character
        module procedure extract_string
    end interface

    interface insert ! Sec. 3.7.2
        module procedure insert_character_into_character
        module procedure insert_character_into_string
        module procedure insert_string_into_character
        module procedure insert_string_into_string
    end interface

    interface remove ! Sec. 3.7.3
        module procedure remove_character
        module procedure remove_string
    end interface

    interface replace ! Sec. 3.7.4
        module procedure replace_character_with_character_start
        module procedure replace_string_with_character_start
        module procedure replace_character_with_string_start
        module procedure replace_string_with_string_start
        module procedure replace_character_with_character_range
        module procedure replace_string_with_character_range
        module procedure replace_character_with_string_range
        module procedure replace_string_with_string_range
        module procedure replace_target_character_with_character_in_character
        module procedure replace_target_character_with_character_in_string
        module procedure replace_target_character_with_string_in_character
        module procedure replace_target_character_with_string_in_string
        module procedure replace_target_string_with_character_in_character
        module procedure replace_target_string_with_character_in_string
        module procedure replace_target_string_with_string_in_character
        module procedure replace_target_string_with_string_in_string
    end interface

    interface split ! Sec. 3.7.5
        module procedure split_character
        module procedure split_string
    end interface
contains
    elemental subroutine assign_character_to_string(lhs, rhs)
        ! Sec. 3.3.1
        type(varying_string), intent(out) :: lhs
        character(len=*), intent(in) :: rhs

        integer :: i
        integer :: length

        length = len(rhs)
        allocate(lhs%characters(length))
        do concurrent (i = 1 : length)
            lhs%characters(i) = rhs(i:i)
        end do
    end subroutine

    elemental subroutine assign_string_to_character(lhs, rhs)
        ! Sec. 3.3.1
        character(len=*), intent(out) :: lhs
        type(varying_string), intent(in) :: rhs

        integer :: i
        integer :: length_input
        integer :: length_output

        length_output = len(lhs)
        if (allocated(rhs%characters)) then
            length_input = size(rhs%characters)
            do concurrent (i = 1 : min(length_input, length_output))
                lhs(i:i) = rhs%characters(i)
            end do
            if (length_input < length_output) then
                do concurrent (i = length_input+1 : length_output)
                    lhs(i:i) = " "
                end do
            end if
        else
            do concurrent (i = 1 : length_output)
                lhs(i:i) = " "
            end do
        end if
    end subroutine

    elemental function concat_strings(lhs, rhs) result(concatenated)
        ! Sec. 3.3.2
        type(varying_string), intent(in) :: lhs
        type(varying_string), intent(in) :: rhs
        type(varying_string) :: concatenated

        concatenated = char(lhs) // char(rhs)
    end function

    elemental function concat_string_and_character(lhs, rhs) result(concatenated)
        ! Sec. 3.3.2
        type(varying_string), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        type(varying_string) :: concatenated

        concatenated = char(lhs) // rhs
    end function

    elemental function concat_character_and_string(lhs, rhs) result(concatenated)
        ! Sec. 3.3.2
        character(len=*), intent(in) :: lhs
        type(varying_string), intent(in) :: rhs
        type(varying_string) :: concatenated

        concatenated = lhs // char(rhs)
    end function

    elemental function string_eq_string(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(varying_string), intent(in) :: lhs
        type(varying_string), intent(in) :: rhs
        logical :: equals

        equals = char(lhs) == char(rhs)
    end function

    elemental function character_eq_string(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        character(len=*), intent(in) :: lhs
        type(varying_string), intent(in) :: rhs
        logical :: equals

        equals = lhs == char(rhs)
    end function

    elemental function string_eq_character(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(varying_string), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: equals

        equals = char(lhs) == rhs
    end function

    elemental function string_ne_string(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(varying_string), intent(in) :: lhs
        type(varying_string), intent(in) :: rhs
        logical :: equals

        equals = char(lhs) /= char(rhs)
    end function

    elemental function character_ne_string(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        character(len=*), intent(in) :: lhs
        type(varying_string), intent(in) :: rhs
        logical :: equals

        equals = lhs /= char(rhs)
    end function

    elemental function string_ne_character(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(varying_string), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: equals

        equals = char(lhs) /= rhs
    end function

    elemental function string_lt_string(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(varying_string), intent(in) :: lhs
        type(varying_string), intent(in) :: rhs
        logical :: equals

        equals = char(lhs) < char(rhs)
    end function

    elemental function character_lt_string(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        character(len=*), intent(in) :: lhs
        type(varying_string), intent(in) :: rhs
        logical :: equals

        equals = lhs < char(rhs)
    end function

    elemental function string_lt_character(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(varying_string), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: equals

        equals = char(lhs) < rhs
    end function

    elemental function string_le_string(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(varying_string), intent(in) :: lhs
        type(varying_string), intent(in) :: rhs
        logical :: equals

        equals = char(lhs) <= char(rhs)
    end function

    elemental function character_le_string(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        character(len=*), intent(in) :: lhs
        type(varying_string), intent(in) :: rhs
        logical :: equals

        equals = lhs <= char(rhs)
    end function

    elemental function string_le_character(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(varying_string), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: equals

        equals = char(lhs) <= rhs
    end function

    elemental function string_gt_string(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(varying_string), intent(in) :: lhs
        type(varying_string), intent(in) :: rhs
        logical :: equals

        equals = char(lhs) > char(rhs)
    end function

    elemental function character_gt_string(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        character(len=*), intent(in) :: lhs
        type(varying_string), intent(in) :: rhs
        logical :: equals

        equals = lhs > char(rhs)
    end function

    elemental function string_gt_character(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(varying_string), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: equals

        equals = char(lhs) > rhs
    end function

    elemental function string_ge_string(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(varying_string), intent(in) :: lhs
        type(varying_string), intent(in) :: rhs
        logical :: equals

        equals = char(lhs) >= char(rhs)
    end function

    elemental function character_ge_string(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        character(len=*), intent(in) :: lhs
        type(varying_string), intent(in) :: rhs
        logical :: equals

        equals = lhs >= char(rhs)
    end function

    elemental function string_ge_character(lhs, rhs) result(equals)
        ! Sec. 3.3.3
        type(varying_string), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: equals

        equals = char(lhs) >= rhs
    end function

    elemental function string_adjustl(string) result(adjusted)
        ! Sec. 3.4.1
        type(varying_string), intent(in) :: string
        type(varying_string) :: adjusted

        adjusted = adjustl(char(string))
    end function

    elemental function string_adjustr(string) result(adjusted)
        ! Sec. 3.4.2
        type(varying_string), intent(in) :: string
        type(varying_string) :: adjusted

        adjusted = adjustr(char(string))
    end function

    pure function string_to_char(string) result(chars)
        ! Sec. 3.4.3
        type(varying_string), intent(in) :: string
        character(len=:), allocatable :: chars

        if (allocated(string%characters)) then
            block
                character(len=size(string%characters)) :: tmp
                tmp = string
                chars = tmp
            end block
          else
            chars = ""
        end if
    end function

    pure function string_to_char_with_length(string, length) result(chars)
        ! Sec. 3.4.3
        type(varying_string), intent(in) :: string
        integer, intent(in) :: length
        character(len=length) :: chars

        if (allocated(string%characters)) then
            chars = string
        end if
    end function

    elemental function string_iachar(c)
        ! Sec. 3.4.4
        type(varying_string), intent(in) :: c
        integer :: string_iachar

        string_iachar = iachar(char(c))
    end function

    elemental function string_ichar(c)
        ! Sec. 3.4.5
        type(varying_string), intent(in) :: c
        integer :: string_ichar

        string_ichar = ichar(char(c))
    end function

    elemental function string_index_string(string, substring, back) result(position)
        ! Sec. 3.4.6
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: substring
        logical, optional, intent(in) :: back
        integer :: position

        position = index(char(string), char(substring), back)
    end function

    elemental function string_index_character(string, substring, back) result(position)
        ! Sec. 3.4.6
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: substring
        logical, optional, intent(in) :: back
        integer :: position

        position = index(char(string), substring, back)
    end function

    elemental function character_index_string(string, substring, back) result(position)
        ! Sec. 3.4.6
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: substring
        logical, optional, intent(in) :: back
        integer :: position

        position = index(string, char(substring), back)
    end function

    elemental function len_string(string) result(length)
        ! Sec. 3.4.7
        type(varying_string), intent(in) :: string
        integer :: length

        length = len(char(string))
    end function

    elemental function len_trim_string(string) result(length)
        ! Sec. 3.4.8
        type(varying_string), intent(in) :: string
        integer :: length

        length = len_trim(char(string))
    end function

    elemental function string_lge_string(string_a, string_b) result(greater_than_or_equals)
        ! Sec 3.4.9
        type(varying_string), intent(in) :: string_a
        type(varying_string), intent(in) :: string_b
        logical :: greater_than_or_equals

        greater_than_or_equals = lge(char(string_a), char(string_b))
    end function

    elemental function character_lge_string(string_a, string_b) result(greater_than_or_equals)
        ! Sec 3.4.9
        character(len=*), intent(in) :: string_a
        type(varying_string), intent(in) :: string_b
        logical :: greater_than_or_equals

        greater_than_or_equals = lge(string_a, char(string_b))
    end function

    elemental function string_lge_character(string_a, string_b) result(greater_than_or_equals)
        ! Sec 3.4.9
        type(varying_string), intent(in) :: string_a
        character(len=*), intent(in) :: string_b
        logical :: greater_than_or_equals

        greater_than_or_equals = lge(char(string_a), string_b)
    end function

    elemental function string_lgt_string(string_a, string_b) result(greater_than)
        ! Sec 3.4.10
        type(varying_string), intent(in) :: string_a
        type(varying_string), intent(in) :: string_b
        logical :: greater_than

        greater_than = lgt(char(string_a), char(string_b))
    end function

    elemental function character_lgt_string(string_a, string_b) result(greater_than)
        ! Sec 3.4.10
        character(len=*), intent(in) :: string_a
        type(varying_string), intent(in) :: string_b
        logical :: greater_than

        greater_than = lgt(string_a, char(string_b))
    end function

    elemental function string_lgt_character(string_a, string_b) result(greater_than)
        ! Sec 3.4.10
        type(varying_string), intent(in) :: string_a
        character(len=*), intent(in) :: string_b
        logical :: greater_than

        greater_than = lgt(char(string_a), string_b)
    end function

    elemental function string_lle_string(string_a, string_b) result(less_than_or_equals)
        ! Sec 3.4.11
        type(varying_string), intent(in) :: string_a
        type(varying_string), intent(in) :: string_b
        logical :: less_than_or_equals

        less_than_or_equals = lle(char(string_a), char(string_b))
    end function

    elemental function character_lle_string(string_a, string_b) result(less_than_or_equals)
        ! Sec 3.4.11
        character(len=*), intent(in) :: string_a
        type(varying_string), intent(in) :: string_b
        logical :: less_than_or_equals

        less_than_or_equals = lle(string_a, char(string_b))
    end function

    elemental function string_lle_character(string_a, string_b) result(less_than_or_equals)
        ! Sec 3.4.11
        type(varying_string), intent(in) :: string_a
        character(len=*), intent(in) :: string_b
        logical :: less_than_or_equals

        less_than_or_equals = lle(char(string_a), string_b)
    end function

    elemental function string_llt_string(string_a, string_b) result(less_than)
        ! Sec 3.4.12
        type(varying_string), intent(in) :: string_a
        type(varying_string), intent(in) :: string_b
        logical :: less_than
        intrinsic :: llt

        less_than = llt(char(string_a), char(string_b))
    end function

    elemental function character_llt_string(string_a, string_b) result(less_than)
        ! Sec 3.4.12
        character(len=*), intent(in) :: string_a
        type(varying_string), intent(in) :: string_b
        intrinsic :: llt
        logical :: less_than

        less_than = llt(string_a, char(string_b))
    end function

    elemental function string_llt_character(string_a, string_b) result(less_than)
        ! Sec 3.4.12
        type(varying_string), intent(in) :: string_a
        character(len=*), intent(in) :: string_b
        intrinsic :: llt
        logical :: less_than

        less_than = llt(char(string_a), string_b)
    end function
    elemental function string_repeat(string, ncopies) result(repeated)
        ! Sec. 3.4.13
        type(varying_string), intent(in) :: string
        integer, intent(in) :: ncopies
        type(varying_string) :: repeated

        intrinsic :: repeat

        if (allocated(string%characters)) then
            block
                character(len=len(string)) :: tmp_char
                tmp_char = string
                repeated = repeat(tmp_char, ncopies)
            end block
        else
            repeated = ""
        end if
    end function

    elemental function string_scan_string(string, set, back) result(position)
        ! Sec. 3.4.14
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: set
        logical, optional, intent(in) :: back
        integer :: position

        position = scan(char(string), char(set), back)
    end function

    elemental function string_scan_character(string, set, back) result(position)
        ! Sec. 3.4.14
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: set
        logical, optional, intent(in) :: back
        integer :: position

        position = scan(char(string), set, back)
    end function

    elemental function character_scan_string(string, set, back) result(position)
        ! Sec. 3.4.14
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: set
        logical, optional, intent(in) :: back
        integer :: position

        position = scan(string, char(set), back)
    end function

    elemental function trim_string(string) result(trimmed)
        ! Sec. 3.4.15
        type(varying_string), intent(in) :: string
        type(varying_string) :: trimmed

        trimmed = trim(char(string))
    end function

    elemental function string_verify_string(string, set, back) result(position)
        ! Sec. 3.5.16
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: set
        logical, optional, intent(in) :: back
        integer :: position

        position = verify(char(string), char(set), back)
    end function

    elemental function string_verify_character(string, set, back) result(position)
        ! Sec. 3.5.16
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: set
        logical, optional, intent(in) :: back
        integer :: position

        position = verify(char(string), set, back)
    end function

    elemental function character_verify_string(string, set, back) result(position)
        ! Sec. 3.5.16
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: set
        logical, optional, intent(in) :: back
        integer :: position

        position = verify(string, char(set), back)
    end function

    elemental function var_str(char)
        ! Sec. 3.5.1
        character(len=*), intent(in) :: char
        type(varying_string) :: var_str

        var_str = char
    end function

    subroutine get_default_unit_to_end_of_record(string, maxlen, iostat)
        ! Sec. 3.6.1
        type(varying_string), intent(out) :: string
        integer, optional, intent(in) :: maxlen
        integer, optional, intent(out) :: iostat

        integer, parameter :: BUFFER_SIZE = 100
        character(len=BUFFER_SIZE) :: buffer
        integer :: next_read_length
        integer :: num_read
        integer :: num_to_read

        if (present(maxlen)) then
            num_to_read = maxlen
        else
            num_to_read = huge(1)
        end if
        string = ""
        if (present(iostat)) then
            do
                if (num_to_read <= 0) exit
                next_read_length = min(BUFFER_SIZE, num_to_read)
                read(*, fmt='(A)', advance='NO', eor=9999, size=num_read, iostat=iostat) buffer(1:next_read_length)
                if (iostat /= 0) return
                string = string // buffer(1:next_read_length)
                num_to_read = num_to_read - next_read_length
            end do
        else
            do
                if (num_to_read <= 0) exit
                next_read_length = min(BUFFER_SIZE, num_to_read)
                read(*, fmt='(A)', advance='NO', eor=9999, size=num_read) buffer(1:next_read_length)
                string = string // buffer(1:next_read_length)
                num_to_read = num_to_read - next_read_length
            end do
        end if
        return
        9999 string = string // buffer(1:num_read)
    end subroutine

    subroutine get_with_unit_to_end_of_record(unit, string, maxlen, iostat)
        ! Sec. 3.6.1
        integer, intent(in) :: unit
        type(varying_string), intent(out) :: string
        integer, optional, intent(in) :: maxlen
        integer, optional, intent(out) :: iostat

        integer, parameter :: BUFFER_SIZE = 100
        character(len=BUFFER_SIZE) :: buffer
        integer :: next_read_length
        integer :: num_read
        integer :: num_to_read

        if (present(maxlen)) then
            num_to_read = maxlen
        else
            num_to_read = huge(1)
        end if
        string = ""
        if (present(iostat)) then
            do
                if (num_to_read <= 0) exit
                next_read_length = min(BUFFER_SIZE, num_to_read)
                read(unit, fmt='(A)', advance='NO', eor=9999, size=num_read, iostat=iostat) buffer(1:next_read_length)
                if (iostat /= 0) return
                string = string // buffer(1:next_read_length)
                num_to_read = num_to_read - next_read_length
            end do
        else
            do
                if (num_to_read <= 0) exit
                next_read_length = min(BUFFER_SIZE, num_to_read)
                read(unit, fmt='(A)', advance='NO', eor=9999, size=num_read) buffer(1:next_read_length)
                string = string // buffer(1:next_read_length)
                num_to_read = num_to_read - next_read_length
            end do
        end if
        return
        9999 string = string // buffer(1:num_read)
    end subroutine

    subroutine get_default_unit_to_terminator_string(string, set, separator, maxlen, iostat)
        ! Sec. 3.6.1
        type(varying_string), intent(out) :: string
        type(varying_string), intent(in) :: set ! possible terminator characters
        type(varying_string), optional, intent(out) :: separator ! actual terminator
        integer, optional, intent(in) :: maxlen
        integer, optional, intent(out) :: iostat

        call get(string, char(set), separator, maxlen, iostat)
    end subroutine

    subroutine get_with_unit_to_terminator_string(unit, string, set, separator, maxlen, iostat)
        ! Sec. 3.6.1
        integer, intent(in) :: unit
        type(varying_string), intent(out) :: string
        type(varying_string), intent(in) :: set ! possible terminator characters
        type(varying_string), optional, intent(out) :: separator ! actual terminator
        integer, optional, intent(in) :: maxlen
        integer, optional, intent(out) :: iostat

        call get(unit, string, char(set), separator, maxlen, iostat)
    end subroutine

    subroutine get_default_unit_to_terminator_characters(string, set, separator, maxlen, iostat)
        ! Sec. 3.6.1
        type(varying_string), intent(out) :: string
        character(len=*), intent(in) :: set ! possible terminator characters
        type(varying_string), optional, intent(out) :: separator ! actual terminator
        integer, optional, intent(in) :: maxlen
        integer, optional, intent(out) :: iostat

        character(len=1) :: buffer
        integer :: num_to_read

        if (present(maxlen)) then
            num_to_read = maxlen
        else
            num_to_read = huge(1)
        end if
        string = ""
        if (present(separator)) separator = ""
        if (present(iostat)) then
            do
                if (num_to_read <= 0) exit
                read(*, fmt='(A)', advance='NO', eor=9999, iostat=iostat) buffer
                if (iostat /= 0) return
                if (index(set, buffer) /= 0) then
                    if (present(separator)) separator = buffer
                    return
                end if
                string = string // buffer
                num_to_read = num_to_read - 1
            end do
        else
            do
                if (num_to_read <= 0) exit
                read(*, fmt='(A)', advance='NO', eor=9999) buffer
                if (index(set, buffer) /= 0) then
                    if (present(separator)) separator = buffer
                    return
                end if
                string = string // buffer
                num_to_read = num_to_read - 1
            end do
        end if
        9999 continue
    end subroutine

    subroutine get_with_unit_to_terminator_characters(unit, string, set, separator, maxlen, iostat)
        ! Sec. 3.6.1
        integer, intent(in) :: unit
        type(varying_string), intent(out) :: string
        character(len=*), intent(in) :: set ! possible terminator characters
        type(varying_string), optional, intent(out) :: separator ! actual terminator
        integer, optional, intent(in) :: maxlen
        integer, optional, intent(out) :: iostat

        character(len=1) :: buffer
        integer :: num_to_read

        if (present(maxlen)) then
            num_to_read = maxlen
        else
            num_to_read = huge(1)
        end if
        string = ""
        if (present(separator)) separator = ""
        if (present(iostat)) then
            do
                if (num_to_read <= 0) exit
                read(unit, fmt='(A)', advance='NO', eor=9999, iostat=iostat) buffer
                if (iostat /= 0) return
                if (index(set, buffer) /= 0) then
                    if (present(separator)) separator = buffer
                    return
                end if
                string = string // buffer
                num_to_read = num_to_read - 1
            end do
        else
            do
                if (num_to_read <= 0) exit
                read(unit, fmt='(A)', advance='NO', eor=9999) buffer
                if (index(set, buffer) /= 0) then
                    if (present(separator)) separator = buffer
                    return
                end if
                string = string // buffer
                num_to_read = num_to_read - 1
            end do
        end if
        9999 continue
    end subroutine

    subroutine put_string_default_unit(string, iostat)
        ! Sec. 3.6.2
        type(varying_string), intent(in) :: string
        integer, optional, intent(out) :: iostat

        call put(char(string), iostat)
    end subroutine

    subroutine put_string_with_unit(unit, string, iostat)
        ! Sec. 3.6.2
        integer, intent(in) :: unit
        type(varying_string), intent(in) :: string
        integer, optional, intent(out) :: iostat

        call put(unit, char(string), iostat)
    end subroutine

    subroutine put_characters_default_unit(string, iostat)
        ! Sec. 3.6.2
        character(len=*), intent(in) :: string
        integer, optional, intent(out) :: iostat

        if (present(iostat)) then
            write(*, fmt='(A)', advance='NO', iostat=iostat) string
        else
            write(*, fmt='(A)', advance='NO') string
        end if
    end subroutine

    subroutine put_characters_with_unit(unit, string, iostat)
        ! Sec. 3.6.2
        integer, intent(in) :: unit
        character(len=*), intent(in) :: string
        integer, optional, intent(out) :: iostat

        if (present(iostat)) then
            write(unit, fmt='(A)', advance='NO', iostat=iostat) string
        else
            write(unit, fmt='(A)', advance='NO') string
        end if
    end subroutine

    subroutine put_line_string_default_unit(string, iostat)
        ! Sec. 3.6.3
        type(varying_string), intent(in) :: string
        integer, optional, intent(out) :: iostat

        call put_line(char(string), iostat)
    end subroutine

    subroutine put_line_string_with_unit(unit, string, iostat)
        ! Sec. 3.6.3
        integer, intent(in) :: unit
        type(varying_string), intent(in) :: string
        integer, optional, intent(out) :: iostat

        call put_line(unit, char(string), iostat)
    end subroutine

    subroutine put_line_characters_default_unit(string, iostat)
        ! Sec. 3.6.3
        character(len=*), intent(in) :: string
        integer, optional, intent(out) :: iostat

        if (present(iostat)) then
            write(*, fmt='(A,/)', advance='NO', iostat=iostat) string
        else
            write(*, fmt='(A,/)', advance='NO') string
        end if
    end subroutine

    subroutine put_line_characters_with_unit(unit, string, iostat)
        ! Sec. 3.6.3
        integer, intent(in) :: unit
        character(len=*), intent(in) :: string
        integer, optional, intent(out) :: iostat

        if (present(iostat)) then
            write(unit, fmt='(A,/)', advance='NO', iostat=iostat) string
        else
            write(unit, fmt='(A,/)', advance='NO') string
        end if
    end subroutine

    elemental function extract_character(string, start, finish) result(extracted)
        ! Sec. 3.7.1
        character(len=*), intent(in) :: string
        integer, optional, intent(in) :: start
        integer, optional, intent(in) :: finish
        type(varying_string) :: extracted

        integer :: start_
        integer :: finish_

        if (present(start)) then
            start_ = max(1, start)
        else
            start_ = 1
        end if
        if (present(finish)) then
            finish_ = min(len(string), finish)
        else
            finish_ = len(string)
        end if

        extracted = string(start_:finish_)
    end function

    elemental function extract_string(string, start, finish) result(extracted)
        ! Sec. 3.7.1
        type(varying_string), intent(in) :: string
        integer, optional, intent(in) :: start
        integer, optional, intent(in) :: finish
        type(varying_string) :: extracted

        extracted = extract(char(string), start, finish)
    end function

    elemental function insert_character_into_character(string, start, substring) result(inserted)
        ! Sec. 3.7.2
        character(len=*), intent(in) :: string
        integer, intent(in) :: start
        character(len=*), intent(in) :: substring
        type(varying_string) :: inserted

        type(varying_string) :: beginning
        type(varying_string) :: middle
        type(varying_string) :: end_

        if (start <= 1) then
            beginning = substring
            middle = string
            end_ = ""
        else if (start > len(string)) then
            beginning = string
            middle = substring
            end_ = ""
        else
            beginning = string(1:start-1)
            middle = substring
            end_ = string(start:)
        end if
        inserted = beginning // middle // end_
    end function

    elemental function insert_character_into_string(string, start, substring) result(inserted)
        ! Sec. 3.7.2
        type(varying_string), intent(in) :: string
        integer, intent(in) :: start
        character(len=*), intent(in) :: substring
        type(varying_string) :: inserted

        inserted = insert(char(string), start, substring)
    end function

    elemental function insert_string_into_character(string ,start, substring) result(inserted)
        ! Sec. 3.7.2
        character(len=*), intent(in) :: string
        integer, intent(in) :: start
        type(varying_string), intent(in) :: substring
        type(varying_string) :: inserted

        inserted = insert(string, start, char(substring))
    end function

    elemental function insert_string_into_string(string ,start, substring) result(inserted)
        ! Sec. 3.7.2
        type(varying_string), intent(in) :: string
        integer, intent(in) :: start
        type(varying_string), intent(in) :: substring
        type(varying_string) :: inserted

        inserted = insert(char(string), start, char(substring))
    end function

    elemental function remove_character(string, start, finish) result(removed)
        ! Sec. 3.7.3
        character(len=*), intent(in) :: string
        integer, optional, intent(in) :: start
        integer, optional, intent(in) :: finish
        type(varying_string) :: removed

        integer :: start_
        integer :: finish_
        type(varying_string) :: beginning
        type(varying_string) :: end_

        if (present(start)) then
            start_ = start
        else
            start_ = 1
        end if
        if (present(finish)) then
            finish_ = finish
        else
            finish_ = len(string)
        end if

        if (start_ > finish_) then
            removed = string
        else
            beginning = string(1:start_ - 1)
            end_ = string(finish_ + 1:len(string))
            removed = beginning // end_
        end if
    end function

    elemental function remove_string(string, start, finish) result(removed)
        ! Sec. 3.7.3
        type(varying_string), intent(in) :: string
        integer, optional, intent(in) :: start
        integer, optional, intent(in) :: finish
        type(varying_string) :: removed

        removed = remove(char(string), start, finish)
    end function

    elemental function replace_character_with_character_start( &
                string, start, substring) result(replaced)
        ! Sec. 3.7.4
        character(len=*), intent(in) :: string
        integer, intent(in) :: start
        character(len=*), intent(in) :: substring
        type(varying_string) :: replaced

        integer :: start_

        start_ = max(1, start)
        replaced = insert( &
                remove(string, start_, start_ + len(substring) - 1), &
                start_, &
                substring)
    end function

    elemental function replace_string_with_character_start( &
            string, start, substring) result(replaced)
        ! Sec. 3.7.4
        type(varying_string), intent(in) :: string
        integer, intent(in) :: start
        character(len=*), intent(in) :: substring
        type(varying_string) :: replaced

        replaced = replace(char(string), start, substring)
    end function

    elemental function replace_character_with_string_start( &
            string, start, substring) result(replaced)
        ! Sec. 3.7.4
        character(len=*), intent(in) :: string
        integer, intent(in) :: start
        type(varying_string), intent(in) :: substring
        type(varying_string) :: replaced

        replaced = replace(string, start, char(substring))
    end function

    elemental function replace_string_with_string_start( &
            string, start, substring) result(replaced)
        ! Sec. 3.7.4
        type(varying_string), intent(in) :: string
        integer, intent(in) :: start
        type(varying_string), intent(in) :: substring
        type(varying_string) :: replaced

        replaced = replace(char(string), start, char(substring))
    end function

    elemental function replace_character_with_character_range( &
            string, start, finish, substring) result(replaced)
        ! Sec. 3.7.4
        character(len=*), intent(in) :: string
        integer, intent(in) :: start
        integer, intent(in) :: finish
        character(len=*), intent(in) :: substring
        type(varying_string) :: replaced

        type(varying_string) :: beginning
        type(varying_string) :: ending

        beginning = string(1 : start-1)
        ending = string(max(finish+1, start) : )
        replaced = beginning // substring // ending
    end function

    elemental function replace_string_with_character_range( &
            string, start, finish, substring) result(replaced)
        ! Sec. 3.7.4
        type(varying_string), intent(in) :: string
        integer, intent(in) :: start
        integer, intent(in) :: finish
        character(len=*), intent(in) :: substring
        type(varying_string) :: replaced

        replaced = replace(char(string), start, finish, substring)
    end function

    elemental function replace_character_with_string_range( &
            string, start, finish, substring) result(replaced)
        ! Sec. 3.7.4
        character(len=*), intent(in) :: string
        integer, intent(in) :: start
        integer, intent(in) :: finish
        type(varying_string), intent(in) :: substring
        type(varying_string) :: replaced

        replaced = replace(string, start, finish, char(substring))
    end function

    elemental function replace_string_with_string_range( &
            string, start, finish, substring) result(replaced)
        ! Sec. 3.7.4
        type(varying_string), intent(in) :: string
        integer, intent(in) :: start
        integer, intent(in) :: finish
        type(varying_string), intent(in) :: substring
        type(varying_string) :: replaced

        replaced = replace(char(string), start, finish, char(substring))
    end function

    elemental function replace_target_character_with_character_in_character( &
            string, target, substring, every, back) result(replaced)
        ! Sec. 3.7.4
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: target
        character(len=*), intent(in) :: substring
        logical, optional, intent(in) :: every
        logical, optional, intent(in) :: back
        type(varying_string) :: replaced

        logical :: back_
        logical :: every_

        if (present(back)) then
            back_ = back
        else
            back_ = .false.
        end if

        if (present(every)) then
            every_ = every
        else
            every_ = .false.
        end if

        if (every_) then
            block
                integer :: i
                integer :: new_length
                character(len=:), allocatable :: new_string
                integer :: num_targets
                integer :: target_length, substring_length
                integer, allocatable :: target_positions(:), substring_positions(:)

                num_targets = get_num_targets(string, target, back_)
                if (num_targets > 0) then
                    target_length = len(target)
                    substring_length = len(substring)
                    new_length = &
                            len(string) &
                            - num_targets*target_length &
                            + num_targets*substring_length
                    allocate(character(len=new_length)::new_string)
                    allocate(target_positions(num_targets))
                    allocate(substring_positions(num_targets))
                    call get_positions( &
                            string, &
                            target, &
                            back_, &
                            num_targets, &
                            substring_length, &
                            target_positions, &
                            substring_positions)
                    if (target_positions(1) > 1) then
                        new_string(1 : substring_positions(1)-1) = string(1 : target_positions(1)-1)
                    end if
                    new_string(substring_positions(1) : substring_positions(1)+substring_length-1) = substring
                    do i = 2, num_targets
                        new_string(substring_positions(i-1)+substring_length : substring_positions(i)-1) = &
                                string(target_positions(i-1)+target_length:target_positions(i)-1)
                        new_string(substring_positions(i) : substring_positions(i)+substring_length-1) = substring
                    end do
                    if (target_positions(num_targets) + target_length <= len(string)) then
                        new_string(substring_positions(num_targets)+substring_length:) = &
                                string(target_positions(num_targets)+target_length:)
                    end if
                    replaced = new_string
                else
                    replaced = string
                end if
            end block
        else
            block
                integer :: position

                position = index(string, target, back_)
                if (position /= 0) then
                    replaced = string(1:position-1) // substring // string(position+len(target):)
                else
                    replaced = string
                end if
            end block
        end if
    contains
        pure function get_num_targets(string_, target_, back__) result(num_targets_)
            character(len=*), intent(in) :: string_
            character(len=*), intent(in) :: target_
            logical, intent(in) :: back__
            integer :: num_targets_

            integer :: len_string
            integer :: len_target
            integer :: prev_start

            len_string = len(string_)
            len_target = len(target_)
            prev_start = index(string_, target_, back__)
            if (prev_start == 0) then
                num_targets_ = 0
            else
                num_targets_ = 1
                if (back__) then
                  do
                      if (prev_start == 1) exit
                      prev_start = index(string_(1:prev_start-1), target_, back__)
                      if (prev_start == 0) then
                          exit
                      else
                          num_targets_ = num_targets_ + 1
                      end if
                  end do
                else
                    do
                        if (prev_start+len_target > len_string) exit
                        if (index(string_(prev_start+len_target:), target_) == 0) then
                            exit
                        else
                            prev_start = index(string_(prev_start+len_target:), target_) + prev_start + len_target - 1
                            num_targets_ = num_targets_ + 1
                        end if
                    end do
                end if
            end if
        end function

        pure subroutine get_positions( &
              string_, &
              target_, &
              back__, &
              num_targets_, &
              len_substring, &
              target_positions_, &
              substring_positions_)
          character(len=*), intent(in) :: string_
          character(len=*), intent(in) :: target_
          logical, intent(in) :: back__
          integer, intent(in) :: num_targets_
          integer, intent(in) :: len_substring
          integer, intent(out) :: target_positions_(:), substring_positions_(:)

          integer :: i, len_target

          len_target = len(target_)
          if (back__) then
              target_positions_(num_targets_) = index(string_, target_, back__)
              do i = num_targets_-1, 1, -1
                  target_positions_(i) = index(string_(1:target_positions_(i+1)-1), target_, back__)
              end do
          else
              target_positions_(1) = index(string_, target_)
              do i = 2, num_targets_
                  target_positions_(i) = &
                          index(string_(target_positions_(i-1)+len_target:), target_) &
                          + target_positions_(i-1) + len_target - 1
              end do
          end if
          substring_positions_(1) = target_positions_(1)
          do i = 2, num_targets_
              substring_positions_(i) = &
                      substring_positions_(i-1) &
                      + (target_positions_(i) - target_positions_(i-1)) &
                      + (len_substring - len_target)
          end do
        end subroutine
    end function replace_target_character_with_character_in_character

    elemental function replace_target_character_with_character_in_string( &
            string, target, substring, every, back) result(replaced)
        ! Sec. 3.7.4
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: target
        character(len=*), intent(in) :: substring
        logical, optional, intent(in) :: every
        logical, optional, intent(in) :: back
        type(varying_string) :: replaced

        replaced = replace(char(string), target, substring, every, back)
    end function

    elemental function replace_target_character_with_string_in_character( &
            string, target, substring, every, back) result(replaced)
        ! Sec. 3.7.4
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: target
        type(varying_string), intent(in) :: substring
        logical, optional, intent(in) :: every
        logical, optional, intent(in) :: back
        type(varying_string) :: replaced

        replaced = replace(string, target, char(substring), every, back)
    end function

    elemental function replace_target_character_with_string_in_string( &
            string, target, substring, every, back) result(replaced)
        ! Sec. 3.7.4
        type(varying_string), intent(in) :: string
        character(len=*), intent(in) :: target
        type(varying_string), intent(in) :: substring
        logical, optional, intent(in) :: every
        logical, optional, intent(in) :: back
        type(varying_string) :: replaced

        replaced = replace(char(string), target, char(substring), every, back)
    end function

    elemental function replace_target_string_with_character_in_character( &
            string, target, substring, every, back) result(replaced)
        ! Sec. 3.7.4
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: target
        character(len=*), intent(in) :: substring
        logical, optional, intent(in) :: every
        logical, optional, intent(in) :: back
        type(varying_string) :: replaced

        replaced = replace(string, char(target), substring, every, back)
    end function

    elemental function replace_target_string_with_character_in_string( &
            string, target, substring, every, back) result(replaced)
        ! Sec. 3.7.4
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: target
        character(len=*), intent(in) :: substring
        logical, optional, intent(in) :: every
        logical, optional, intent(in) :: back
        type(varying_string) :: replaced

        replaced = replace(char(string), char(target), substring, every, back)
    end function

    elemental function replace_target_string_with_string_in_character( &
            string, target, substring, every, back) result(replaced)
        ! Sec. 3.7.4
        character(len=*), intent(in) :: string
        type(varying_string), intent(in) :: target
        type(varying_string), intent(in) :: substring
        logical, optional, intent(in) :: every
        logical, optional, intent(in) :: back
        type(varying_string) :: replaced

        replaced = replace(string, char(target), char(substring), every, back)
    end function

    elemental function replace_target_string_with_string_in_string( &
            string, target, substring, every, back) result(replaced)
        ! Sec. 3.7.4
        type(varying_string), intent(in) :: string
        type(varying_string), intent(in) :: target
        type(varying_string), intent(in) :: substring
        logical, optional, intent(in) :: every
        logical, optional, intent(in) :: back
        type(varying_string) :: replaced

        replaced = replace(char(string), char(target), char(substring), every, back)
    end function

    elemental subroutine split_character(string, word, set, separator, back)
        ! Sec. 3.7.5
        type(varying_string), intent(inout) :: string
        type(varying_string), intent(out) :: word
        character(len=*), intent(in) :: set
        type(varying_string), optional, intent(out) :: separator
        logical, optional, intent(in) :: back

        logical :: backwards
        integer :: i
        integer :: string_length
        character(len=:), allocatable :: temp

        allocate(character(len=0) :: temp) ! TODO: remove once gfortran bug is fixed
        string_length = len(string)
        if (present(back)) then
            backwards = back
        else
            backwards = .false.
        end if
        if (backwards) then
            do i = string_length, 1, -1
                if (index(set, extract(string, i, i)) /= 0) exit
            end do
            if (i < 1) then
                word = string
                string = ""
                if (present(separator)) separator = ""
            else
                word = extract(string, i+1)
                temp = char(extract(string, 1, i-1))
                if (present(separator)) separator = extract(string, i, i)
                string = temp
            end if
        else
            do i = 1, string_length
                if (index(set, extract(string, i, i)) /= 0) exit
            end do
            if (i > string_length) then
                word = string
                string = ""
                if (present(separator)) separator = ""
            else
                word = extract(string, 1, i-1)
                temp = char(extract(string, i+1))
                if (present(separator)) separator = extract(string, i, i)
                string = temp
            end if
        end if
    end subroutine

    elemental subroutine split_string(string, word, set, separator, back)
        ! Sec. 3.7.5
        type(varying_string), intent(inout) :: string
        type(varying_string), intent(out) :: word
        type(varying_string), intent(in) :: set
        type(varying_string), optional, intent(out) :: separator
        logical, optional, intent(in) :: back

        call split(string, word, char(set), separator, back)
    end subroutine
end module
