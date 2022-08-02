module garden_result_m
    use iso_varying_string, only: varying_string, assignment(=), var_str
    use strff, only: join, NEWLINE
    use garden_individual_result_m, only: individual_result_t

    implicit none
    private
    public :: result_t, fail, succeed

    type :: result_t
        private
        type(individual_result_t), allocatable :: results(:)
    contains
        private
        procedure :: combine_results
        generic, public :: operator(.and.) => combine_results
        procedure, public :: num_asserts
        procedure, public :: num_failing_asserts
        procedure, public :: passed
        procedure, public :: failure_description
        procedure, public :: verbose_description
    end type

    interface fail
        module procedure fail_c
        module procedure fail_s
    end interface

    interface succeed
        module procedure succeed_c
        module procedure succeed_s
    end interface
contains
    pure function fail_c(message) result(failure)
        character(len=*), intent(in) :: message
        type(result_t) :: failure

        failure = fail(var_str(message))
    end function

    pure function fail_s(message) result(failure)
        type(varying_string), intent(in) :: message
        type(result_t) :: failure

        allocate(failure%results(1))
        failure%results(1) = individual_result_t(message, .false.)
    end function

    pure function succeed_c(message) result(success)
        character(len=*), intent(in) :: message
        type(result_t) :: success

        success = succeed(var_str(message))
    end function

    pure function succeed_s(message) result(success)
        type(varying_string), intent(in) :: message
        type(result_t) :: success

        allocate(success%results(1))
        success%results(1) = individual_result_t(message, .true.)
    end function

    pure function combine_results(lhs, rhs) result(combined)
        class(result_t), intent(in) :: lhs
        type(result_t), intent(in) :: rhs
        type(result_t) :: combined

        integer :: num_lhs
        integer :: num_rhs

        if (allocated(lhs%results) .and. allocated(rhs%results)) then
            num_lhs = size(lhs%results)
            num_rhs = size(rhs%results)
            allocate(combined%results(num_lhs + num_rhs))
            combined%results(1:num_lhs) = lhs%results(:)
            combined%results(num_lhs+1:) = rhs%results(:)
        else if (allocated(lhs%results)) then
            combined = lhs
        else if (allocated(rhs%results)) then
            combined = rhs
        end if
    end function

    pure function failure_description(self, colorize) result(description)
        class(result_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(varying_string) :: description

        logical, allocatable :: failed(:)
        type(varying_string), allocatable :: failed_messages(:)
        integer :: num_failed
        integer :: i, j

        if (self%passed()) then
            description = ""
        else
            allocate(failed, source = .not.self%results%passed())
            num_failed = count(failed)
            j = 1
            allocate(failed_messages(num_failed))
            do i = 1, size(failed)
                if (failed(i)) then
                    failed_messages(j) = self%results(i)%failure_description(colorize)
                    j = j + 1
                    if (j > num_failed) exit
                end if
            end do
            description = join(failed_messages, NEWLINE)
        end if
    end function

    pure function num_asserts(self)
        class(result_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = size(self%results)
    end function

    pure function num_failing_asserts(self) result(num_asserts)
        class(result_t), intent(in) :: self
        integer :: num_asserts

        num_asserts = count(.not.self%results%passed())
    end function

    pure function passed(self)
        class(result_t), intent(in) :: self
        logical :: passed

        passed = all(self%results%passed())
    end function

    pure function verbose_description(self, colorize) result(description)
        class(result_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(varying_string) :: description

        description = join(self%results%verbose_description(colorize), NEWLINE)
    end function
end module
