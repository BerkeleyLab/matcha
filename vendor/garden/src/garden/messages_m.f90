module garden_messages_m
    use iso_varying_string, only: &
            varying_string, operator(==), operator(//), var_str
    use strff, only: add_hanging_indentation, indent, NEWLINE
    use garden_constants_m, only: INDENTATION

    implicit none
    private
    public :: &
            delimit, &
            make_doesnt_include_failure_message, &
            make_doesnt_include_success_message, &
            make_empty_failure_message, &
            make_equals_failure_message, &
            make_equals_success_message, &
            make_faster_than_failure_message, &
            make_faster_than_success_message, &
            make_includes_failure_message, &
            make_includes_success_message, &
            make_within_failure_message, &
            make_within_success_message, &
            with_user_message, &
            EMPTY_SUCCESS_MESSAGE, &
            NOT_FAILURE_MESSAGE, &
            NOT_SUCCESS_MESSAGE, &
            THAT_FAILURE_MESSAGE, &
            THAT_SUCCESS_MESSAGE

    interface delimit
        module procedure delimit_c
        module procedure delimit_s
    end interface

    interface make_doesnt_include_failure_message
        module procedure make_doesnt_include_failure_message_cc
        module procedure make_doesnt_include_failure_message_cs
        module procedure make_doesnt_include_failure_message_sc
        module procedure make_doesnt_include_failure_message_ss
    end interface

    interface make_doesnt_include_success_message
        module procedure make_doesnt_include_success_message_cc
        module procedure make_doesnt_include_success_message_cs
        module procedure make_doesnt_include_success_message_sc
        module procedure make_doesnt_include_success_message_ss
    end interface

    interface make_empty_failure_message
        module procedure make_empty_failure_message_c
        module procedure make_empty_failure_message_s
    end interface

    interface make_equals_failure_message
        module procedure make_equals_failure_message_cc
        module procedure make_equals_failure_message_cs
        module procedure make_equals_failure_message_sc
        module procedure make_equals_failure_message_ss
    end interface

    interface make_equals_success_message
        module procedure make_equals_success_message_c
        module procedure make_equals_success_message_s
    end interface

    interface make_faster_than_failure_message
        module procedure make_faster_than_failure_message_ccc
        module procedure make_faster_than_failure_message_ccs
        module procedure make_faster_than_failure_message_csc
        module procedure make_faster_than_failure_message_css
        module procedure make_faster_than_failure_message_scc
        module procedure make_faster_than_failure_message_scs
        module procedure make_faster_than_failure_message_ssc
        module procedure make_faster_than_failure_message_sss
    end interface

    interface make_faster_than_success_message
        module procedure make_faster_than_success_message_ccc
        module procedure make_faster_than_success_message_ccs
        module procedure make_faster_than_success_message_csc
        module procedure make_faster_than_success_message_css
        module procedure make_faster_than_success_message_scc
        module procedure make_faster_than_success_message_scs
        module procedure make_faster_than_success_message_ssc
        module procedure make_faster_than_success_message_sss
    end interface

    interface make_includes_failure_message
        module procedure make_includes_failure_message_cc
        module procedure make_includes_failure_message_cs
        module procedure make_includes_failure_message_sc
        module procedure make_includes_failure_message_ss
    end interface

    interface make_includes_success_message
        module procedure make_includes_success_message_cc
        module procedure make_includes_succes_message_cs
        module procedure make_includes_succes_message_sc
        module procedure make_includes_succes_message_ss
    end interface

    interface make_within_failure_message
        module procedure make_within_failure_message_ccc
        module procedure make_within_failure_message_ccs
        module procedure make_within_failure_message_csc
        module procedure make_within_failure_message_css
        module procedure make_within_failure_message_scc
        module procedure make_within_failure_message_scs
        module procedure make_within_failure_message_ssc
        module procedure make_within_failure_message_sss
    end interface

    interface make_within_success_message
        module procedure make_within_success_message_ccc
        module procedure make_within_success_message_ccs
        module procedure make_within_success_message_csc
        module procedure make_within_success_message_css
        module procedure make_within_success_message_scc
        module procedure make_within_success_message_scs
        module procedure make_within_success_message_ssc
        module procedure make_within_success_message_sss
    end interface

    interface with_user_message
        module procedure with_user_message_cc
        module procedure with_user_message_cs
        module procedure with_user_message_sc
        module procedure with_user_message_ss
    end interface

    character(len=*), parameter :: EMPTY_SUCCESS_MESSAGE = "String was empty"
    character(len=*), parameter :: NOT_FAILURE_MESSAGE = "Expected to not be true"
    character(len=*), parameter :: NOT_SUCCESS_MESSAGE = "Was not true"
    character(len=*), parameter :: THAT_FAILURE_MESSAGE = "Expected to be true"
    character(len=*), parameter :: THAT_SUCCESS_MESSAGE = "Was true"
contains
    pure function delimit_c(string) result(delimited)
        character(len=*), intent(in) :: string
        type(varying_string) :: delimited

        delimited = delimit(var_str(string))
    end function

    pure function delimit_s(string) result(delimited)
        type(varying_string), intent(in) :: string
        type(varying_string) :: delimited

        delimited = "|" // string // "|"
    end function

    pure function make_doesnt_include_failure_message_cc( &
            search_for, string) result(message)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string) :: message

        message = make_doesnt_include_failure_message( &
                var_str(search_for), var_str(string))
    end function

    pure function make_doesnt_include_failure_message_cs( &
            search_for, string) result(message)
        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string) :: message

        message = make_doesnt_include_failure_message( &
                var_str(search_for), string)
    end function

    pure function make_doesnt_include_failure_message_sc( &
            search_for, string) result(message)
        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string) :: message

        message = make_doesnt_include_failure_message( &
                search_for, var_str(string))
    end function

    pure function make_doesnt_include_failure_message_ss( &
            search_for, string) result(message)
        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string) :: message

        message = add_hanging_indentation( &
                "Expected" // NEWLINE &
                    // indent( &
                            delimit(add_hanging_indentation(string, 1)), &
                            INDENTATION) // NEWLINE &
                    // "to not include" // NEWLINE &
                    // indent( &
                            delimit(add_hanging_indentation(search_for, 1)), &
                            INDENTATION), &
                INDENTATION)
    end function

    pure function make_doesnt_include_success_message_cc( &
            search_for, string) result(message)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string) :: message

        message = make_doesnt_include_success_message( &
                var_str(search_for), var_str(string))
    end function

    pure function make_doesnt_include_success_message_cs( &
            search_for, string) result(message)
        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string) :: message

        message = make_doesnt_include_success_message( &
                var_str(search_for), string)
    end function

    pure function make_doesnt_include_success_message_sc( &
            search_for, string) result(message)
        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string) :: message

        message = make_doesnt_include_success_message( &
                search_for, var_str(string))
    end function

    pure function make_doesnt_include_success_message_ss( &
            search_for, string) result(message)
        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string) :: message

        message = add_hanging_indentation( &
                "The string" // NEWLINE &
                    // indent( &
                            delimit(add_hanging_indentation(string, 1)), &
                            INDENTATION) // NEWLINE &
                    // "did not include" // NEWLINE &
                    // indent( &
                            delimit(add_hanging_indentation(search_for, 1)), &
                            INDENTATION), &
                INDENTATION)
    end function

    pure function make_empty_failure_message_c(string) result(message)
        character(len=*), intent(in) :: string
        type(varying_string) :: message

        message = make_empty_failure_message(var_str(string))
    end function

    pure function make_empty_failure_message_s(string) result(message)
        type(varying_string), intent(in) :: string
        type(varying_string) :: message

        message = add_hanging_indentation( &
                "The string" // NEWLINE &
                    // indent( &
                            delimit(add_hanging_indentation(string, 1)), &
                            INDENTATION) // NEWLINE &
                    // "wasn't empty", &
                INDENTATION)
    end function

    pure function make_equals_failure_message_cc(expected, actual) result(message)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(varying_string) :: message

        message = make_equals_failure_message(var_str(expected), var_str(actual))
    end function

    pure function make_equals_failure_message_cs(expected, actual) result(message)
        character(len=*), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(varying_string) :: message

        message = make_equals_failure_message(var_str(expected), actual)
    end function

    pure function make_equals_failure_message_sc(expected, actual) result(message)
        type(varying_string), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(varying_string) :: message

        message = make_equals_failure_message(expected, var_str(actual))
    end function

    pure function make_equals_failure_message_ss(expected, actual) result(message)
        type(varying_string), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(varying_string) :: message

        message = add_hanging_indentation( &
                "Expected" // NEWLINE &
                    // indent( &
                            delimit(add_hanging_indentation(expected, 1)), &
                            INDENTATION) // NEWLINE &
                    // "but got" // NEWLINE &
                    // indent( &
                            delimit(add_hanging_indentation(actual, 1)), &
                            INDENTATION), &
                INDENTATION)
    end function

    pure function make_equals_success_message_c(expected) result(message)
        character(len=*), intent(in) :: expected
        type(varying_string) :: message

        message = make_equals_success_message(var_str(expected))
    end function

    pure function make_equals_success_message_s(expected) result(message)
        type(varying_string), intent(in) :: expected
        type(varying_string) :: message

        message = add_hanging_indentation( &
                "Expected and got" // NEWLINE &
                    // indent( &
                            delimit(add_hanging_indentation(expected, 1)), &
                            INDENTATION), &
                INDENTATION)
    end function

    pure function make_faster_than_failure_message_ccc( &
            reference, actual, iterations) result(message)
        character(len=*), intent(in) :: reference
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_failure_message( &
                var_str(reference), var_str(actual), var_str(iterations))
    end function

    pure function make_faster_than_failure_message_ccs( &
            reference, actual, iterations) result(message)
        character(len=*), intent(in) :: reference
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_failure_message( &
                var_str(reference), var_str(actual), iterations)
    end function

    pure function make_faster_than_failure_message_csc( &
            reference, actual, iterations) result(message)
        character(len=*), intent(in) :: reference
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_failure_message( &
                var_str(reference), actual, var_str(iterations))
    end function

    pure function make_faster_than_failure_message_css( &
            reference, actual, iterations) result(message)
        character(len=*), intent(in) :: reference
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_failure_message( &
                var_str(reference), actual, iterations)
    end function

    pure function make_faster_than_failure_message_scc( &
            reference, actual, iterations) result(message)
        type(varying_string), intent(in) :: reference
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_failure_message( &
                reference, var_str(actual), var_str(iterations))
    end function

    pure function make_faster_than_failure_message_scs( &
            reference, actual, iterations) result(message)
        type(varying_string), intent(in) :: reference
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_failure_message( &
                reference, var_str(actual), iterations)
    end function

    pure function make_faster_than_failure_message_ssc( &
            reference, actual, iterations) result(message)
        type(varying_string), intent(in) :: reference
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_failure_message( &
                reference, actual, var_str(iterations))
    end function

    pure function make_faster_than_failure_message_sss( &
            reference, actual, iterations) result(message)
        type(varying_string), intent(in) :: reference
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: iterations
        type(varying_string) :: message

        message = &
                "Computation took " // actual &
                // ", which was slower than the reference time of " &
                // reference // ", averaged over " // iterations // " iterations."
    end function

    pure function make_faster_than_success_message_ccc( &
            reference, actual, iterations) result(message)
        character(len=*), intent(in) :: reference
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_success_message( &
                var_str(reference), var_str(actual), var_str(iterations))
    end function

    pure function make_faster_than_success_message_ccs( &
            reference, actual, iterations) result(message)
        character(len=*), intent(in) :: reference
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_success_message( &
                var_str(reference), var_str(actual), iterations)
    end function

    pure function make_faster_than_success_message_csc( &
            reference, actual, iterations) result(message)
        character(len=*), intent(in) :: reference
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_success_message( &
                var_str(reference), actual, var_str(iterations))
    end function

    pure function make_faster_than_success_message_css( &
            reference, actual, iterations) result(message)
        character(len=*), intent(in) :: reference
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_success_message( &
                var_str(reference), actual, iterations)
    end function

    pure function make_faster_than_success_message_scc( &
            reference, actual, iterations) result(message)
        type(varying_string), intent(in) :: reference
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_success_message( &
                reference, var_str(actual), var_str(iterations))
    end function

    pure function make_faster_than_success_message_scs( &
            reference, actual, iterations) result(message)
        type(varying_string), intent(in) :: reference
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_success_message( &
                reference, var_str(actual), iterations)
    end function

    pure function make_faster_than_success_message_ssc( &
            reference, actual, iterations) result(message)
        type(varying_string), intent(in) :: reference
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: iterations
        type(varying_string) :: message

        message = make_faster_than_success_message( &
                reference, actual, var_str(iterations))
    end function

    pure function make_faster_than_success_message_sss( &
            reference, actual, iterations) result(message)
        type(varying_string), intent(in) :: reference
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: iterations
        type(varying_string) :: message

        message = &
                "Computation took " // actual &
                // ", which was faster than the reference time of " &
                // reference // ", averaged over " // iterations // " iterations."
    end function

    pure function make_includes_failure_message_cc(search_for, string) result(message)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string) :: message

        message = make_includes_failure_message( &
                var_str(search_for), var_str(string))
    end function

    pure function make_includes_failure_message_cs(search_for, string) result(message)
        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string) :: message

        message = make_includes_failure_message(var_str(search_for), string)
    end function

    pure function make_includes_failure_message_sc(search_for, string) result(message)
        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string) :: message

        message = make_includes_failure_message(search_for, var_str(string))
    end function

    pure function make_includes_failure_message_ss(search_for, string) result(message)
        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string) :: message

        message = add_hanging_indentation( &
                "Expected" // NEWLINE &
                    // indent( &
                            delimit(add_hanging_indentation(string, 1)), &
                            INDENTATION) // NEWLINE &
                    // "to include" // NEWLINE &
                    // indent( &
                            delimit(add_hanging_indentation(search_for, 1)), &
                            INDENTATION), &
                INDENTATION)
    end function

    pure function make_includes_success_message_cc(search_for, string) result(message)
        character(len=*), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string) :: message

        message = make_includes_success_message( &
                var_str(search_for), var_str(string))
    end function

    pure function make_includes_succes_message_cs(search_for, string) result(message)
        character(len=*), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string) :: message

        message = make_includes_success_message(var_str(search_for), string)
    end function

    pure function make_includes_succes_message_sc(search_for, string) result(message)
        type(varying_string), intent(in) :: search_for
        character(len=*), intent(in) :: string
        type(varying_string) :: message

        message = make_includes_success_message(search_for, var_str(string))
    end function

    pure function make_includes_succes_message_ss(search_for, string) result(message)
        type(varying_string), intent(in) :: search_for
        type(varying_string), intent(in) :: string
        type(varying_string) :: message

        message = add_hanging_indentation( &
                "The string" // NEWLINE &
                    // indent( &
                            delimit(add_hanging_indentation(string, 1)), &
                            INDENTATION) // NEWLINE &
                    // "included" // NEWLINE &
                    // indent( &
                            delimit(add_hanging_indentation(search_for, 1)), &
                            INDENTATION), &
                INDENTATION)
    end function

    pure function make_within_failure_message_ccc( &
            expected, actual, tolerance) result(message)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_failure_message( &
                var_str(expected), var_str(actual), var_str(tolerance))
    end function

    pure function make_within_failure_message_ccs( &
            expected, actual, tolerance) result(message)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_failure_message( &
                var_str(expected), var_str(actual), tolerance)
    end function

    pure function make_within_failure_message_csc( &
            expected, actual, tolerance) result(message)
        character(len=*), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_failure_message( &
                var_str(expected), actual, var_str(tolerance))
    end function

    pure function make_within_failure_message_css( &
            expected, actual, tolerance) result(message)
        character(len=*), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_failure_message( &
                var_str(expected), actual, tolerance)
    end function

    pure function make_within_failure_message_scc( &
            expected, actual, tolerance) result(message)
        type(varying_string), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_failure_message( &
                expected, var_str(actual), var_str(tolerance))
    end function

    pure function make_within_failure_message_scs( &
            expected, actual, tolerance) result(message)
        type(varying_string), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_failure_message( &
                expected, var_str(actual), tolerance)
    end function

    pure function make_within_failure_message_ssc( &
            expected, actual, tolerance) result(message)
        type(varying_string), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_failure_message( &
                expected, actual, var_str(tolerance))
    end function

    pure function make_within_failure_message_sss( &
            expected, actual, tolerance) result(message)
        type(varying_string), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: tolerance
        type(varying_string) :: message

        message = add_hanging_indentation( &
                "Expected" // NEWLINE &
                    // indent( &
                            delimit(add_hanging_indentation(actual, 1)), &
                            INDENTATION) // NEWLINE &
                    // "to be within " // delimit("±" // tolerance) // " of" // NEWLINE &
                    // indent( &
                            delimit(add_hanging_indentation(expected, 1)), &
                            INDENTATION), &
                INDENTATION)
    end function

    pure function make_within_success_message_ccc( &
            expected, actual, tolerance) result(message)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_success_message( &
                var_str(expected), var_str(actual), var_str(tolerance))
    end function

    pure function make_within_success_message_ccs( &
            expected, actual, tolerance) result(message)
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_success_message( &
                var_str(expected), var_str(actual), tolerance)
    end function

    pure function make_within_success_message_csc( &
            expected, actual, tolerance) result(message)
        character(len=*), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_success_message( &
                var_str(expected), actual, var_str(tolerance))
    end function

    pure function make_within_success_message_css( &
            expected, actual, tolerance) result(message)
        character(len=*), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_success_message( &
                var_str(expected), actual, tolerance)
    end function

    pure function make_within_success_message_scc( &
            expected, actual, tolerance) result(message)
        type(varying_string), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_success_message( &
                expected, var_str(actual), var_str(tolerance))
    end function

    pure function make_within_success_message_scs( &
            expected, actual, tolerance) result(message)
        type(varying_string), intent(in) :: expected
        character(len=*), intent(in) :: actual
        type(varying_string), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_success_message( &
                expected, var_str(actual), tolerance)
    end function

    pure function make_within_success_message_ssc( &
            expected, actual, tolerance) result(message)
        type(varying_string), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        character(len=*), intent(in) :: tolerance
        type(varying_string) :: message

        message = make_within_success_message( &
                expected, actual, var_str(tolerance))
    end function

    pure function make_within_success_message_sss( &
            expected, actual, tolerance) result(message)
        type(varying_string), intent(in) :: expected
        type(varying_string), intent(in) :: actual
        type(varying_string), intent(in) :: tolerance
        type(varying_string) :: message

        message = add_hanging_indentation( &
                "The value" // NEWLINE &
                    // indent( &
                            delimit(add_hanging_indentation(actual, 1)), &
                            INDENTATION) // NEWLINE &
                    // "was within " // delimit("±" // tolerance) // " of" // NEWLINE &
                    // indent( &
                            delimit(add_hanging_indentation(expected, 1)), &
                            INDENTATION), &
                INDENTATION)
    end function

    pure function with_user_message_cc(message, user_message) result(whole_message)
        character(len=*), intent(in) :: message
        character(len=*), intent(in) :: user_message
        type(varying_string) :: whole_message

        whole_message = with_user_message( &
                var_str(message), var_str(user_message))
    end function

    pure function with_user_message_cs(message, user_message) result(whole_message)
        character(len=*), intent(in) :: message
        type(varying_string), intent(in) :: user_message
        type(varying_string) :: whole_message

        whole_message = with_user_message(var_str(message), user_message)
    end function

    pure function with_user_message_sc(message, user_message) result(whole_message)
        type(varying_string), intent(in) :: message
        character(len=*), intent(in) :: user_message
        type(varying_string) :: whole_message

        whole_message = with_user_message( &
                message, var_str(user_message))
    end function

    pure function with_user_message_ss(message, user_message) result(whole_message)
        type(varying_string), intent(in) :: message
        type(varying_string), intent(in) :: user_message
        type(varying_string) :: whole_message

        if (user_message == "") then
            whole_message = message
        else
            whole_message = &
                    message // NEWLINE &
                    // indent( &
                            add_hanging_indentation( &
                                    "User Message:" // NEWLINE &
                                        // delimit(add_hanging_indentation( &
                                                user_message, 1)), &
                                    INDENTATION), &
                            INDENTATION)
        end if
    end function
end module
