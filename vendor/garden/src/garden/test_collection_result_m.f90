module garden_test_collection_result_m
    use iso_varying_string, only: varying_string, assignment(=), operator(//)
    use strff, only: add_hanging_indentation, join, NEWLINE
    use garden_constants_m, only: INDENTATION
    use garden_test_result_m, only: test_result_t
    use garden_test_result_item_m, only: test_result_item_t

    implicit none
    private
    public :: test_collection_result_t

    type, extends(test_result_t) :: test_collection_result_t
        private
        type(varying_string) :: description
        type(test_result_item_t), allocatable :: results(:)
    contains
        private
        procedure, public :: num_asserts
        procedure, public :: num_cases
        procedure, public :: num_failing_asserts
        procedure, public :: num_failing_cases
        procedure, public :: passed
        procedure, public :: failure_description
        procedure, public :: verbose_description
    end type

    interface test_collection_result_t
        module procedure constructor
    end interface
contains
    pure function constructor(description, results) result(test_collection_result)
        type(varying_string), intent(in) :: description
        type(test_result_item_t), intent(in) :: results(:)
        type(test_collection_result_t) :: test_collection_result

        test_collection_result%description = description
        allocate(test_collection_result%results, source = results)
    end function

    pure recursive function failure_description( &
            self, colorize) result(description)
        class(test_collection_result_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(varying_string) :: description

        logical, allocatable :: failed(:)
        type(varying_string), allocatable :: failed_messages(:)
        integer :: num_failed
        integer :: i, j

        if (self%passed()) then
            description = ""
        else
            allocate(failed, source = [(.not.self%results(i)%passed(), i = 1, size(self%results))])
            num_failed = count(failed)
            j= 1
            allocate(failed_messages(num_failed))
            do i = 1, size(failed)
                if (failed(i)) then
                    failed_messages(j) = self%results(i)%failure_description(colorize)
                    j = j + 1
                    if (j > num_failed) exit
                end if
            end do
            description = add_hanging_indentation( &
                    self%description // NEWLINE // join( &
                            failed_messages, &
                            NEWLINE), &
                    INDENTATION)
        end if
    end function

    pure recursive function num_asserts(self)
        class(test_collection_result_t), intent(in) :: self
        integer :: num_asserts

        integer :: i

        num_asserts = sum([(self%results(i)%num_asserts(), i = 1, size(self%results))])
    end function

    pure recursive function num_cases(self)
        class(test_collection_result_t), intent(in) :: self
        integer :: num_cases

        integer :: i

        num_cases = sum([(self%results(i)%num_cases(), i = 1, size(self%results))])
    end function

    pure recursive function num_failing_asserts(self) result(num_asserts)
        class(test_collection_result_t), intent(in) :: self
        integer :: num_asserts

        integer :: i

        num_asserts = sum([(self%results(i)%num_failing_asserts(), i = 1, size(self%results))])
    end function

    pure recursive function num_failing_cases(self) result(num_cases)
        class(test_collection_result_t), intent(in) :: self
        integer :: num_cases

        integer :: i

        num_cases = sum([(self%results(i)%num_failing_cases(), i = 1, size(self%results))])
    end function

    pure recursive function passed(self)
        class(test_collection_result_t), intent(in) :: self
        logical :: passed

        integer :: i

        passed = all([(self%results(i)%passed(), i = 1, size(self%results))])
    end function

    pure recursive function verbose_description( &
            self, colorize) result(description)
        class(test_collection_result_t), intent(in) :: self
        logical, intent(in) :: colorize
        type(varying_string) :: description

        integer :: i

        description = add_hanging_indentation( &
                self%description // NEWLINE // join( &
                        [(self%results(i)%verbose_description(colorize), i = 1, size(self%results))], &
                        NEWLINE), &
                INDENTATION)
    end function
end module
