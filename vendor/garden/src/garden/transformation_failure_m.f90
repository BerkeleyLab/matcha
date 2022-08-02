module garden_transformation_failure_m
    use garden_input_m, only: input_t
    use garden_result_m, only: result_t

    implicit none
    private
    public :: transformation_failure_t

    type, extends(input_t) :: transformation_failure_t
        private
        type(result_t) :: result__
    contains
        private
        procedure, public :: result_
    end type

    interface transformation_failure_t
        module procedure constructor
    end interface
contains
    pure function constructor(result_) result(transformation_failure)
        type(result_t), intent(in) :: result_
        type(transformation_failure_t) :: transformation_failure

        transformation_failure%result__ = result_
    end function

    pure function result_(self)
        class(transformation_failure_t), intent(in) :: self
        type(result_t) :: result_

        result_ = self%result__
    end function
end module
