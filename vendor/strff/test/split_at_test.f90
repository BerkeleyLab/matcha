module split_at_test
    use iso_varying_string, only: varying_string
    use strff, only: split_at
    use veggies, only: test_item_t, result_t, assert_equals, describe, it

    implicit none
    private

    public :: test_split_at
contains
    function test_split_at() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "split_at", &
                [ it( &
                        "returns the same string when split on a character it doesn't contain", &
                        check_split_doesnt_contain) &
                , it( &
                        "can split strings at something", check_split_at_something) &
                , it( &
                        "includes an empty string at the end if the string ends in a separator", &
                        check_empty_end) &
                , it( &
                        "includes an empty string at the beginning if the string begins with a separator", &
                        check_empty_begin) &
                , it( &
                        "returns the same string when given no split characters", &
                        check_no_split_characters) &
                , it( &
                        "includes an empty string between split characters", &
                        check_empty_between) &
                , it( &
                        "returns an array with a single empty string when given an empty string", &
                        check_for_empty_string) &
                , it( &
                        "returns an array of empty strings when given a string that only contains split characters", &
                        check_for_only_split_characters) &
                ])
    end function

    pure function check_split_doesnt_contain() result(result_)
        type(result_t) :: result_

        type(varying_string), allocatable :: strings(:)

        allocate(strings(0)) ! TODO: remove once bug in gfortran has been fixed

        strings = split_at("Hello World", ",")
        result_ = &
                assert_equals(1, size(strings)) &
                .and.assert_equals("Hello World", strings(1))
    end function

    pure function check_split_at_something() result(result_)
        type(result_t) :: result_

        type(varying_string), allocatable :: strings(:)

        allocate(strings(0)) ! TODO: remove once bug in gfortran has been fixed

        strings = split_at("Hello,World", ",")
        result_ = &
                assert_equals(2, size(strings)) &
                .and.assert_equals("Hello", strings(1)) &
                .and.assert_equals("World", strings(2))
    end function

    pure function check_empty_end() result(result_)
        type(result_t) :: result_

        type(varying_string), allocatable :: strings(:)

        allocate(strings(0)) ! TODO: remove once bug in gfortran has been fixed

        strings = split_at("Hello,World,", ",")
        result_ = &
                assert_equals(3, size(strings)) &
                .and.assert_equals("Hello", strings(1)) &
                .and.assert_equals("World", strings(2)) &
                .and.assert_equals("", strings(3))
    end function

    pure function check_empty_begin() result(result_)
        type(result_t) :: result_

        type(varying_string), allocatable :: strings(:)

        allocate(strings(0)) ! TODO: remove once bug in gfortran has been fixed

        strings = split_at(",Hello,World", ",")
        result_ = &
                assert_equals(3, size(strings)) &
                .and.assert_equals("", strings(1)) &
                .and.assert_equals("Hello", strings(2)) &
                .and.assert_equals("World", strings(3))
    end function

    pure function check_no_split_characters() result(result_)
        type(result_t) :: result_

        type(varying_string), allocatable :: strings(:)

        allocate(strings(0)) ! TODO: remove once bug in gfortran has been fixed

        strings = split_at("Hello,World", "")
        result_ = &
                assert_equals(1, size(strings)) &
                .and.assert_equals("Hello,World", strings(1))
    end function

    pure function check_empty_between() result(result_)
        type(result_t) :: result_

        type(varying_string), allocatable :: strings(:)

        allocate(strings(0)) ! TODO: remove once bug in gfortran has been fixed

        strings = split_at("Hello, World", " ,")
        result_ = &
                assert_equals(3, size(strings)) &
                .and.assert_equals("Hello", strings(1)) &
                .and.assert_equals("", strings(2)) &
                .and.assert_equals("World", strings(3))
    end function

    pure function check_for_empty_string() result(result_)
        type(result_t) :: result_

        type(varying_string), allocatable :: strings(:)

        allocate(strings(0)) ! TODO: remove once bug in gfortran has been fixed

        strings = split_at("", " ,")
        result_ = &
                assert_equals(1, size(strings)) &
                .and.assert_equals("", strings(1))
    end function

    pure function check_for_only_split_characters() result(result_)
        type(result_t) :: result_

        type(varying_string), allocatable :: strings(:)

        allocate(strings(0)) ! TODO: remove once bug in gfortran has been fixed

        strings = split_at(", ", " ,")
        result_ = &
                assert_equals(3, size(strings)) &
                .and.assert_equals("", strings(1)) &
                .and.assert_equals("", strings(2)) &
                .and.assert_equals("", strings(3))
    end function
end module
