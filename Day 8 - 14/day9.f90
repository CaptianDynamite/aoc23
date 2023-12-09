module day9helper
    implicit none

contains
    subroutine test()
        print *, "Please let 9 be chill"
    end subroutine test
end module day9helper

program day9
    use day9helper
    implicit none
    call test()
end program day9