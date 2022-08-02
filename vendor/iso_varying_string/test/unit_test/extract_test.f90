module extract_test
    use iso_varying_string, only: extract, var_str
    use veggies, only: &
            result_t, test_item_t, assert_empty, assert_equals, describe, it

    implicit none
    private
    public :: test_extract_character, test_extract_string
contains
    function test_extract_character() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.7.1 EXTRACT character", &
                [ it( &
                        "The result value is a copy of the characters of the argument" &
                        // " string between positions start and finish, inclusive.", &
                        check_extract_character) &
                , it( &
                        "If start is absent, the value one is used for start.", &
                        check_extract_character_without_start) &
                , it( &
                        "If start is less than one, the value one is used for start.", &
                        check_extract_character_with_start_lt_one) &
                , it( &
                        "If finish is absent, the value LEN(string) is used for finish.", &
                        check_extract_character_without_finish) &
                , it( &
                        "If finish is greater than LEN(string), the value LEN(string) is used for finish.", &
                        check_extract_character_with_finish_gt_len_string) &
                , it( &
                        "If finish is less than start, the result is a zero-length string.", &
                        check_extract_character_zero_length) &
                ])
    end function

    function test_extract_string() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.7.1 EXTRACT string", &
                [ it( &
                        "The result value is a copy of the characters of the argument" &
                        // " string between positions start and finish, inclusive.", &
                        check_extract_string) &
                , it( &
                        "If start is absent, the value one is used for start.", &
                        check_extract_string_without_start) &
                , it( &
                        "If start is less than one, the value one is used for start.", &
                        check_extract_string_with_start_lt_one) &
                , it( &
                        "If finish is absent, the value LEN(string) is used for finish.", &
                        check_extract_string_without_finish) &
                , it( &
                        "If finish is greater than LEN(string), the value LEN(string) is used for finish.", &
                        check_extract_string_with_finish_gt_len_string) &
                , it( &
                        "If finish is less than start, the result is a zero-length string.", &
                        check_extract_string_zero_length) &
                ])
    end function

    pure function check_extract_character() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assert_equals(example(2:4), extract(example, 2, 4))
    end function

    pure function check_extract_character_without_start() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assert_equals(example(1:4), extract(example, finish = 4))
    end function

    pure function check_extract_character_with_start_lt_one() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assert_equals(example(1:4), extract(example, -1, 4))
    end function

    pure function check_extract_character_without_finish() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assert_equals(example(2:), extract(example, 2))
    end function

    pure function check_extract_character_with_finish_gt_len_string() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assert_equals(example(2:), extract(example, 2, len(example) + 1))
    end function

    pure function check_extract_character_zero_length() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assert_empty(extract(example, 10, -2))
    end function

    pure function check_extract_string() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assert_equals(example(2:4), extract(var_str(example), 2, 4))
    end function

    pure function check_extract_string_without_start() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assert_equals(example(1:4), extract(var_str(example), finish = 4))
    end function

    pure function check_extract_string_with_start_lt_one() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assert_equals(example(1:4), extract(var_str(example), -1, 4))
    end function

    pure function check_extract_string_without_finish() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assert_equals(example(2:), extract(var_str(example), 2))
    end function

    pure function check_extract_string_with_finish_gt_len_string() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assert_equals(example(2:), extract(var_str(example), 2, len(example) + 1))
    end function

    pure function check_extract_string_zero_length() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: example = "EXAMPLE"

        result_ = assert_empty(extract(var_str(example), 10, -2))
    end function
end module
