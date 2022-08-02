program string_put
    use iso_varying_string, only: put, var_str

    implicit none

    call put(var_str("hello from string_put"))
end program
