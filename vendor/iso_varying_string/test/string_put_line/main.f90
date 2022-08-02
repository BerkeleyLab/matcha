program string_put_line
    use iso_varying_string, only: put_line, var_str

    implicit none

    call put_line(var_str("hello from string_put_line"))
end program
