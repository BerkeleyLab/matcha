program simple_get
    use iso_varying_string, only: &
            varying_string, operator(//), operator(/=), get, put_line

    implicit none

    type(varying_string) :: string

    call get(string)
    if (string /= "hello simple_get") then
        call put_line("expected 'hello simple_get' but got '" // string // "'")
        error stop
    end if
end program
