module result_test
    use garden, only: &
            result_t, &
            test_item_t, &
            assert_doesnt_include, &
            assert_equals, &
            assert_includes, &
            assert_that, &
            assert_not, &
            describe, &
            fail, &
            it, &
            succeed

    implicit none
    private

    public :: test_result
contains
    function test_result() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Results", &
                [ it("Can tell whether they passed", check_passed) &
                , it( &
                        "Can tell how many assertions there were", &
                        check_num_asserts) &
                , it( &
                        "Can tell how many failing assertions there were", &
                        check_num_failing_asserts) &
                , it( &
                        "Verbose description includes all the messages", &
                        check_verbose_includes) &
                , it( &
                        "Failure description only includes the failing messages", &
                        check_failure_includes) &
                ])
    end function

    pure function check_passed() result(result_)
        type(result_t) :: result_

        type(result_t) :: failing_result
        type(result_t) :: passing_result

        passing_result = succeed("Message")
        failing_result = fail("Message")

        result_ = &
                assert_that(passing_result%passed()) &
                .and.assert_not(failing_result%passed())
    end function

    pure function check_num_asserts() result(result_)
        type(result_t) :: result_

        type(result_t) :: multiple_asserts

        multiple_asserts = succeed("First").and.succeed("Second")

        result_ = assert_equals(2, multiple_asserts%num_asserts())
    end function

    pure function check_num_failing_asserts() result(result_)
        type(result_t) :: result_

        type(result_t) :: multiple_asserts

        multiple_asserts = succeed("First").and.fail("Second")

        result_ = assert_equals(1, multiple_asserts%num_failing_asserts())
    end function

    pure function check_verbose_includes() result(result_)
        type(result_t) :: result_

        type(result_t) :: multiple_asserts

        multiple_asserts = succeed("First").and.fail("Second")

        result_ = &
                assert_includes( &
                        "First", multiple_asserts%verbose_description(.false.))&
                .and.assert_includes( &
                        "Second", multiple_asserts%verbose_description(.false.))
    end function

    pure function check_failure_includes() result(result_)
        type(result_t) :: result_

        type(result_t) :: multiple_asserts

        multiple_asserts = succeed("First").and.fail("Second")

        result_ = &
                assert_doesnt_include( &
                        "First", multiple_asserts%failure_description(.false.))&
                .and.assert_includes( &
                        "Second", multiple_asserts%failure_description(.false.))
    end function
end module
