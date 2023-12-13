module day13helper
    implicit none
contains
    subroutine test(string)
        character(len=*), intent(in) :: string
        print *, string
    end subroutine test
end module day13helper

program day13
    use day13helper
    implicit none

    character(len=512) :: cur_line ! Arbitrary but large enough
    integer :: iostat

    open(24, file='day13.txt', status='old')
    do while (.true.)
        read(24, '(A)', iostat=iostat) cur_line
        if (iostat /= 0) then
            exit
        end if
        call test(trim(cur_line))
    end do
    close(24)

end program day13