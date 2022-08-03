program get_iostat
    use iso_fortran_env, only: IOSTAT_EOR, IOSTAT_END
    use iso_varying_string, only: varying_string, get, put_line

    integer :: stat
    type(varying_string) :: string

    call get(string, iostat = stat)
    if (stat /= IOSTAT_EOR) then
        call put_line("didn't get EOR")
        error stop
    end if
    call get(string, iostat = stat)
    if (stat /= IOSTAT_END) then
        call put_line("didn't get EOF")
        error stop
    end if
end program
