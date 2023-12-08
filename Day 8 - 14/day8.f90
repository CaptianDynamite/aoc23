module day8helper
    implicit none
contains
    subroutine test()
        print *, "Please let day 8 be a Fortran W"
    end subroutine test
end module day8helper

program day8
    use day8helper
    implicit none
    call test()
end program day8