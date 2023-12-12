module day12helper
    implicit none
    contains
    subroutine test()
        print *, "Please be easy today, I only have 45 mins!"
    end subroutine test
end module day12helper

program day12
    use day12helper
    implicit none
    call test()
end program day12