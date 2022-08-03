program get_maxlen
    use iso_varying_string, only: &
            varying_string, operator(//), operator(/=), get, put_line

    implicit none

    type(varying_string) :: hello
    type(varying_string) :: remaining

    call get(hello, 5)
    if (hello /= "hello") then
        call put_line("expected 'hello' but got '" // hello // "'")
        error stop
    end if
    call get(remaining)
    if (remaining /= " get_maxlen") then
        call put_line("expected ' get_maxlen' but got '" // remaining // "'")
        error stop
    end if
end program
