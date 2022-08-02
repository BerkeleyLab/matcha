module insert_test
    use iso_varying_string, only: insert, var_str
    use veggies, only: result_t, test_item_t, assert_equals, describe, it

    implicit none
    private
    public :: &
            test_insert_character_into_character, &
            test_insert_character_into_string, &
            test_insert_string_into_character, &
            test_insert_string_into_string
contains
    function test_insert_character_into_character() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.7.2: INSERT character into character", &
                [ it( &
                        "The result value is a copy of the characters of the argument" &
                        // " string with the characters of substring inserted into the" &
                        // " copy of string before the character at the position start.", &
                        check_insert_character_into_character) &
                , it( &
                        "If start is greater than LEN(string), then substring is appended to the copy of string", &
                        check_insert_character_into_character_at_end) &
                , it( &
                        "If start is less than one, then substring is prepended to the copy of string", &
                        check_insert_character_into_character_at_beginning) &
                ])
    end function

    function test_insert_character_into_string() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.7.2: INSERT character into string", &
                [ it( &
                        "The result value is a copy of the characters of the argument" &
                        // " string with the characters of substring inserted into the" &
                        // " copy of string before the character at the position start.", &
                        check_insert_character_into_string) &
                , it( &
                        "If start is greater than LEN(string), then substring is appended to the copy of string", &
                        check_insert_character_into_string_at_end) &
                , it( &
                        "If start is less than one, then substring is prepended to the copy of string", &
                        check_insert_character_into_string_at_beginning) &
                ])
    end function

    function test_insert_string_into_character() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.7.2: INSERT string into character", &
                [ it( &
                        "The result value is a copy of the characters of the argument" &
                        // " string with the characters of substring inserted into the" &
                        // " copy of string before the character at the position start.", &
                        check_insert_string_into_character) &
                , it( &
                        "If start is greater than LEN(string), then substring is appended to the copy of string", &
                        check_insert_string_into_character_at_end) &
                , it( &
                        "If start is less than one, then substring is prepended to the copy of string", &
                        check_insert_string_into_character_at_beginning) &
                ])
    end function

    function test_insert_string_into_string() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.7.2: INSERT string into string", &
                [ it( &
                        "The result value is a copy of the characters of the argument" &
                        // " string with the characters of substring inserted into the" &
                        // " copy of string before the character at the position start.", &
                        check_insert_string_into_string) &
                , it( &
                        "If start is greater than LEN(string), then substring is appended to the copy of string", &
                        check_insert_string_into_string_at_end) &
                , it( &
                        "If start is less than one, then substring is prepended to the copy of string", &
                        check_insert_string_into_string_at_beginning) &
                ])
    end function

    pure function check_insert_character_into_character() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: string = "STRING"
        character(len=*), parameter :: substring = "SUBSTRING"
        character(len=*), parameter :: expected = "STRSUBSTRINGING"
        integer, parameter :: start = 4

        result_ = assert_equals(expected, insert(string, start, substring))
    end function

    pure function check_insert_character_into_character_at_end() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: string = "STRING"
        character(len=*), parameter :: substring = "SUBSTRING"
        character(len=*), parameter :: expected = "STRINGSUBSTRING"
        integer, parameter :: start = 7

        result_ = assert_equals(expected, insert(string, start, substring))
    end function

    pure function check_insert_character_into_character_at_beginning() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: string = "STRING"
        character(len=*), parameter :: substring = "SUBSTRING"
        character(len=*), parameter :: expected = "SUBSTRINGSTRING"
        integer, parameter :: start = -1

        result_ = assert_equals(expected, insert(string, start, substring))
    end function

    pure function check_insert_character_into_string() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: string = "STRING"
        character(len=*), parameter :: substring = "SUBSTRING"
        character(len=*), parameter :: expected = "STRSUBSTRINGING"
        integer, parameter :: start = 4

        result_ = assert_equals(expected, insert(var_str(string), start, substring))
    end function

    pure function check_insert_character_into_string_at_end() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: string = "STRING"
        character(len=*), parameter :: substring = "SUBSTRING"
        character(len=*), parameter :: expected = "STRINGSUBSTRING"
        integer, parameter :: start = 7

        result_ = assert_equals(expected, insert(var_str(string), start, substring))
    end function

    pure function check_insert_character_into_string_at_beginning() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: string = "STRING"
        character(len=*), parameter :: substring = "SUBSTRING"
        character(len=*), parameter :: expected = "SUBSTRINGSTRING"
        integer, parameter :: start = -1

        result_ = assert_equals(expected, insert(var_str(string), start, substring))
    end function

    pure function check_insert_string_into_character() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: string = "STRING"
        character(len=*), parameter :: substring = "SUBSTRING"
        character(len=*), parameter :: expected = "STRSUBSTRINGING"
        integer, parameter :: start = 4

        result_ = assert_equals(expected, insert(string, start, var_str(substring)))
    end function

    pure function check_insert_string_into_character_at_end() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: string = "STRING"
        character(len=*), parameter :: substring = "SUBSTRING"
        character(len=*), parameter :: expected = "STRINGSUBSTRING"
        integer, parameter :: start = 7

        result_ = assert_equals(expected, insert(string, start, var_str(substring)))
    end function

    pure function check_insert_string_into_character_at_beginning() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: string = "STRING"
        character(len=*), parameter :: substring = "SUBSTRING"
        character(len=*), parameter :: expected = "SUBSTRINGSTRING"
        integer, parameter :: start = -1

        result_ = assert_equals(expected, insert(string, start, var_str(substring)))
    end function

    pure function check_insert_string_into_string() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: string = "STRING"
        character(len=*), parameter :: substring = "SUBSTRING"
        character(len=*), parameter :: expected = "STRSUBSTRINGING"
        integer, parameter :: start = 4

        result_ = assert_equals(expected, insert(var_str(string), start, var_str(substring)))
    end function

    pure function check_insert_string_into_string_at_end() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: string = "STRING"
        character(len=*), parameter :: substring = "SUBSTRING"
        character(len=*), parameter :: expected = "STRINGSUBSTRING"
        integer, parameter :: start = 7

        result_ = assert_equals(expected, insert(var_str(string), start, var_str(substring)))
    end function

    pure function check_insert_string_into_string_at_beginning() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: string = "STRING"
        character(len=*), parameter :: substring = "SUBSTRING"
        character(len=*), parameter :: expected = "SUBSTRINGSTRING"
        integer, parameter :: start = -1

        result_ = assert_equals(expected, insert(var_str(string), start, var_str(substring)))
    end function
end module
