module garden_command_line_m
    use iso_fortran_env, only: error_unit, output_unit
    use iso_varying_string, only: varying_string, assignment(=), put_line
    use strff, only: NEWLINE

#ifdef USE_CAFFEINE
   use caffeine_m, only : stop => caf_stop, error stop => caf_error_stop
#endif

    implicit none
    private
    public :: &
            options_t, &
            get_options, &
            DEBUG, &
            MAX_SHRINK_ATTEMPTS, &
            NUM_GENERATOR_TESTS

    type :: options_t
        private
        logical :: colorize_
        logical :: quiet_
        logical :: verbose_
        logical :: filter_tests_
        type(varying_string) :: filter_string_
    contains
        private
        procedure, public :: colorize
        procedure, public :: quiet
        procedure, public :: verbose
        procedure, public :: filter_tests
        procedure, public :: filter_string
    end type

    logical, protected :: DEBUG = .false.
    integer, protected :: MAX_SHRINK_ATTEMPTS = 100
    integer, protected :: NUM_GENERATOR_TESTS = 100
contains
    function get_options() result(options)
        type(options_t) :: options

        character(len=100) :: argument
        character(len=100) :: program_name
        integer :: i
        integer :: iostat
        integer :: num_arguments

        options%colorize_ = .true.
        options%quiet_ = .false.
        options%verbose_ = .false.
        options%filter_tests_ = .false.
        options%filter_string_ = ""

        call get_command_argument(0, program_name)
        num_arguments = command_argument_count()
        i = 1
        do while (i <= num_arguments)
            call get_command_argument(i, argument)
            select case (trim(argument))
            case ("-c", "--color-off")
                options%colorize_ = .false.
            case ("-h", "--help")
                call put_line(output_unit, usageMessage(program_name))
                stop
            case ("-f", "--filter")
                options%filter_tests_ = .true.
                i = i + 1
                call get_command_argument(i, argument)
                options%filter_string_ = trim(argument)
            case ("-n", "--numrand")
                i = i + 1
                call get_command_argument(i, argument)
                read(argument, *, iostat=iostat) NUM_GENERATOR_TESTS
                if (iostat /= 0) then
                    call put_line( &
                            error_unit, &
                            'Unable to read "' // trim(argument) // '" as an integer' // NEWLINE)
                    call put_line(error_unit, usageMessage(program_name))
                    error stop
                end if
                if (NUM_GENERATOR_TESTS <= 0) then
                    call put_line(error_unit, "Number of random values must be >0")
                    error stop
                end if
            case ("-s", "--shrink-max")
                i = i + 1
                call get_command_argument(i, argument)
                read(argument, *, iostat=iostat) MAX_SHRINK_ATTEMPTS
                if (iostat /= 0) then
                    call put_line( &
                            error_unit, &
                            'Unable to read "' // trim(argument) // '" as an integer' // NEWLINE)
                    call put_line(error_unit, usageMessage(program_name))
                    error stop
                end if
            case ("-q", "--quiet")
                options%quiet_ = .true.
            case ("-v", "--verbose")
                options%verbose_ = .true.
            case ("-d", "--debug")
                DEBUG = .true.
            case default
                call put_line( &
                        error_unit, &
                        "Unknown argument: '" // trim(argument) // "'" // NEWLINE)
                call put_line(error_unit, usageMessage(program_name))
                error stop
            end select
            i = i + 1
        end do
    contains
        pure function usageMessage(program_name_)
            character(len=*), intent(in) :: program_name_
            type(varying_string) :: usageMessage

            usageMessage = &
                    "Usage: " // trim(program_name_) // " [-h] [-q] [-v] [-d] [-f string] [-n num] [-s num] [-c]" // NEWLINE &
                    // "  options:" // NEWLINE &
                    // "    -h, --help                    Output this message and exit" // NEWLINE &
                    // "    -q, --quiet                   Don't print the test descriptions before" // NEWLINE &
                    // "                                  running the tests" // NEWLINE &
                    // "    -v, --verbose                 Print all of the assertion messages, not" // NEWLINE &
                    // "                                  just the failing ones" // NEWLINE &
                    // "    -d, --debug                   Report the beginning and end of execution" // NEWLINE &
                    // "                                  of each test case or suite" // NEWLINE & 
                    // "    -f string, --filter string    Only run cases or collections whose" // NEWLINE &
                    // "                                  description contains the given string" // NEWLINE &
                    // "    -n num, --numrand num         Number of random values to use for each" // NEWLINE &
                    // "                                  test with generated values (default = 100)" // NEWLINE &
                    // "    -s num, --shrink-max num      Number of attempts to find a simpler value" // NEWLINE &
                    // "                                  if a random value fails (default = 100)" // NEWLINE &
                    // "    -c, --color-off               Don't colorize the output"
        end function
    end function

    pure function colorize(self)
        class(options_t), intent(in) :: self
        logical :: colorize

        colorize = self%colorize_
    end function

    pure function quiet(self)
        class(options_t), intent(in) :: self
        logical :: quiet

        quiet = self%quiet_
    end function

    pure function verbose(self)
        class(options_t), intent(in) :: self
        logical :: verbose

        verbose = self%verbose_
    end function

    pure function filter_tests(self)
        class(options_t), intent(in) :: self
        logical :: filter_tests

        filter_tests = self%filter_tests_
    end function

    pure function filter_string(self)
        class(options_t), intent(in) :: self
        type(varying_string) :: filter_string

        filter_string = self%filter_string_
    end function
end module
