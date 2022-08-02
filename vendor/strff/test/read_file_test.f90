module read_file_test
    use iso_varying_string, only: put
    use strff, only: read_file, NEWLINE
    use veggies, only: test_item_t, result_t, assert_equals, describe, it

    implicit none
    private

    public :: test_read_file
contains
    function test_read_file() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "read_file", &
                [ it("gets the contents from the file", check_read_file) &
                ! The performance of one implementation over another appears to be
                ! based heavily on platform, so we don't bother testing it for now.
                !, it( &
                !         "is faster than an alternative implementation", check_speed) &
                ])
    end function

    function check_read_file() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: FILE_CONTENTS = &
                "Just" // NEWLINE &
                // "Some" // NEWLINE &
                // "Contents"
        character(len=*), parameter :: TEMP_FILE_NAME = "read_file_tmp.txt"
        integer :: file_unit

        open(newunit = file_unit, file = TEMP_FILE_NAME, action = "WRITE", status = "REPLACE")
        call put(file_unit, FILE_CONTENTS)
        close(file_unit)

        result_ = assert_equals(FILE_CONTENTS, read_file(TEMP_FILE_NAME))

        open(newunit = file_unit, file = TEMP_FILE_NAME)
        close(file_unit, status = "DELETE")
    end function

    ! function check_speed() result(result_)
    !     use iso_varying_string, only: varying_string, put
    !     use text_m, only: TEST_TEXT
    !     use veggies, only: result_t, assert_faster_than
    !
    !     type(result_t) :: result_
    !
    !     character(len=*), parameter :: TEMP_FILE_NAME = "read_file_speed_tmp.txt"
    !     type(varying_string) :: contents
    !     integer :: file_unit
    !
    !     open(newunit = file_unit, file = TEMP_FILE_NAME, action = "WRITE", status = "REPLACE")
    !     call put(file_unit, TEST_TEXT)
    !     close(file_unit)
    !
    !     result_ = assert_faster_than(do_alt_read, do_fast_read, 50)
    !
    !     open(newunit = file_unit, file = TEMP_FILE_NAME)
    !     close(file_unit, status = "DELETE")
    ! contains
    !     subroutine do_fast_read
    !         use strff, only: read_file
    !
    !         contents = read_file(TEMP_FILE_NAME)
    !     end subroutine
    !
    !     subroutine do_alt_read
    !         contents = alt_read_file(TEMP_FILE_NAME)
    !     end subroutine
    ! end function
    !
    ! function alt_read_file(filename) result(contents)
    !     use iso_fortran_env, only: IOSTAT_END
    !     use iso_varying_string, only: varying_string, get
    !     use strff, only: join, NEWLINE
    !
    !     character(len=*), intent(in) :: filename
    !     type(varying_string) :: contents
    !
    !     integer :: file_unit
    !     integer :: i
    !     type(varying_string), allocatable :: lines(:)
    !     integer :: num_lines
    !     integer :: stat
    !     type(varying_string) :: tmp
    !
    !     open(newunit = file_unit, file = filename, action = "READ", status = "OLD")
    !     num_lines = 0
    !     do
    !         call get(file_unit, tmp, iostat = stat)
    !         if (stat == IOSTAT_END) exit
    !         num_lines = num_lines + 1
    !     end do
    !     rewind(file_unit)
    !
    !     allocate(lines(num_lines))
    !     do i = 1, num_lines
    !         call get(file_unit, lines(i))
    !     end do
    !     close(file_unit)
    !
    !     contents = join(lines, NEWLINE)
    ! end function
end module
