module day11helper
    implicit none
contains
    subroutine test()
        print *, "Please let tomorrow be easy, I have things to be doing!"
    end subroutine test
end module day11helper

program day11
    use day11helper
    implicit none
    call test()
end program day11