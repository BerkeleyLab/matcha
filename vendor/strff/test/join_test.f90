module join_test
    use iso_varying_string, only: var_str
    use strff, only: join
    use veggies, only: test_item_t, result_t, assert_equals, describe, it

    implicit none
    private

    public :: test_join
contains
    function test_join() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "join", &
                [ it( &
                        "for only one string returns that string", check_join_one) &
                , it( &
                        "puts multiple strings together separated by the given string", &
                        check_join_multiple) &
                ])
    end function test_join

    pure function check_join_one() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: EXAMPLE = "Example"

        result_ = assert_equals(EXAMPLE, join([var_str(EXAMPLE)], "anything"))
    end function

    pure function check_join_multiple() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "Hello, again, world", &
                join([var_str("Hello"), var_str("again"), var_str("world")], ", "))
    end function
end module
