program get_terminator
    use iso_varying_string, only: &
            varying_string, operator(//), operator(/=), get, put_line

    implicit none

    type(varying_string) :: string
    type(varying_string) :: separator

    call get(string, ", ", separator)
    if (string /= "hello") then
        call put_line("expected 'hello' but got '" // string // "'")
        error stop
    end if
    if (separator /= ",") then
        call put_line("separator should have been ',' but was '" // separator // "'")
        error stop
    end if
    call get(string, ", ", separator)
    if (string /= "get") then
        call put_line("expected 'get' but got '" // string // "'")
        error stop
    end if
    if (separator /= " ") then
        call put_line("separator should have been ' ' but was '" // separator // "'")
        error stop
    end if
    call get(string, ", ", separator)
    if (string /= "terminator") then
        call put_line("expected 'terminator' but got '" // string // "'")
        error stop
    end if
    if (separator /= "") then
        call put_line("separator should have been empty, but was '" // separator // "'")
        error stop
    end if
end program
