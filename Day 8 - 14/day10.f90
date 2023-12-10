module day10helper
    implicit none
contains
    subroutine test()
        print *, "Let's go, day 10!"
    end subroutine test
end module day10helper

program day10
    use day10helper
    implicit none
    call test()
end program day10