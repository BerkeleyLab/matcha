module tcelsim
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, tcelsim!"
  end subroutine say_hello
end module tcelsim
