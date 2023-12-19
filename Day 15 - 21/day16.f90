module day16helper
    use iso_fortran_env, only: int64
    implicit none

    type :: pos
        integer :: x, y
    end type pos

    enum, bind(c)
        enumerator :: none  = 0
        enumerator :: up    = 1
        enumerator :: down  = 2
        enumerator :: left  = 3
        enumerator :: right = 4
    end enum

    type :: pos_history
        logical :: dir(4)
    end type pos_history

    type :: vec
        type(pos) :: position
        integer   :: direction
    contains
        procedure :: next
    end type vec

    ! I wanted to use PDTs here, but gfortran's being a royal pain and blocking my every attempt.
    type :: vec_stack
        type(vec) :: vecs(100000) ! Arbitrary but long enough
        integer   :: ptr = 0
    contains
        procedure :: push
        procedure :: pop
    end type vec_stack

contains
    impure function energized(grid, start) result(amount)
        character, intent(in) :: grid(:, :)
        type(vec), intent(in) :: start
        integer :: amount
        type(pos_history), allocatable :: seen(:, :)
        logical, allocatable :: illuminated(:, :)
        type(vec_stack) :: splits_to_visit
        type(vec) :: cur
        allocate(seen(size(grid, 1), size(grid, 2)))
        allocate(illuminated(size(grid, 1), size(grid, 2)))
        seen = pos_history([.false., .false., .false., .false.])
        illuminated = .false.
        cur = start
        splits_to_visit = vec_stack(vec(pos(0, 0), none))
        do while (cur%direction /= none .or. splits_to_visit%ptr /= 0)
            if (cur%direction == none .and. splits_to_visit%ptr /= 0) then
                call splits_to_visit%pop(cur)
            elseif (cur%direction == none .and. splits_to_visit%ptr == 0) then
                exit
            elseif (seen(cur%position%x, cur%position%y)%dir(cur%direction)) then
                cur = vec(pos(0, 0), none)
                cycle
            end if
            illuminated(cur%position%x, cur%position%y) = .true.
            select case (grid(cur%position%x, cur%position%y))
            case ('.')
                seen(cur%position%x, cur%position%y)%dir(cur%direction) = .true.
                seen(cur%position%x, cur%position%y)%dir(opp(cur%direction)) = .true.
                ! Travelling the reverse direction is identical
            case ('/')
                seen(cur%position%x, cur%position%y)%dir(cur%direction) = .true.
                seen(cur%position%x, cur%position%y)%dir(opp(opp_fs(cur%direction))) = .true.
                cur%direction = opp_fs(cur%direction)
            case ('\')
                seen(cur%position%x, cur%position%y)%dir(cur%direction) = .true.
                seen(cur%position%x, cur%position%y)%dir(opp(opp_bs(cur%direction))) = .true.
                cur%direction = opp_bs(cur%direction)
            case ('|')
                if (cur%direction == left .or. cur%direction == right) then
                    ! All directions are covered, except for down, which will be covered later.
                    seen(cur%position%x, cur%position%y)%dir(up) = .true.
                    seen(cur%position%x, cur%position%y)%dir(left) = .true.
                    seen(cur%position%x, cur%position%y)%dir(right) = .true.
                    call splits_to_visit%push(vec( cur%position, down ))
                    cur%direction = up
                else
                    seen(cur%position%x, cur%position%y)%dir(up) = .true.
                    seen(cur%position%x, cur%position%y)%dir(down) = .true.
                end if
            case ('-')
                if (cur%direction == up .or. cur%direction == down) then
                    ! All directions are covered, except for right, which will be covered later.
                    seen(cur%position%x, cur%position%y)%dir(up) = .true.
                    seen(cur%position%x, cur%position%y)%dir(down) = .true.
                    seen(cur%position%x, cur%position%y)%dir(left) = .true.
                    call splits_to_visit%push(vec( cur%position, right ))
                    cur%direction = left
                else
                    seen(cur%position%x, cur%position%y)%dir(left) = .true.
                    seen(cur%position%x, cur%position%y)%dir(right) = .true.
                end if
            end select
            cur = cur%next(pos(size(grid, 1), size(grid, 2)))
        end do
        amount = count(illuminated)
        deallocate(seen)
        deallocate(illuminated)
    end function energized

    function opp(dir_in) result(dir_out)
        integer, intent(in) :: dir_in
        integer :: dir_out
        select case (dir_in)
            case (none)
                stop 1
            case (up)
                dir_out = down
            case (down)
                dir_out = up
            case (left)
                dir_out = right
            case (right)
                dir_out = left
        end select
    end function opp

    function opp_bs(dir_in) result(dir_out)
        integer, intent(in) :: dir_in
        integer :: dir_out
        select case (dir_in)
            case (none)
                stop 1
            case (up)
                dir_out = left
            case (down)
                dir_out = right
            case (left)
                dir_out = up
            case (right)
                dir_out = down
        end select
    end function opp_bs

    function opp_fs(dir_in) result(dir_out)
        integer, intent(in) :: dir_in
        integer :: dir_out
        select case (dir_in)
            case (none)
                stop 1
            case (up)
                dir_out = right
            case (down)
                dir_out = left
            case (left)
                dir_out = down
            case (right)
                dir_out = up
        end select
    end function opp_fs

    pure function next(self, grid_lims) result(out)
        class(vec), intent(in) :: self
        type(pos), intent(in)  :: grid_lims
        type(pos) :: cur
        type(vec) :: out
        cur = self%position
        out = vec(pos(0, 0), none)
        select case(self%direction)
            case (up)
                if (cur%y - 1 >= 1) then
                    out = vec(pos(cur%x, cur%y - 1), up)
                end if
            case (down)
                if (cur%y + 1 <= grid_lims%y) then
                    out = vec(pos(cur%x, cur%y + 1), down)
                end if
            case (left)
                if (cur%x - 1 >= 1) then
                    out = vec(pos(cur%x - 1, cur%y), left)
                end if
            case (right)
                if (cur%x + 1 <= grid_lims%x) then
                    out = vec(pos(cur%x + 1, cur%y), right)
                end if
        end select
    end function next

    subroutine push(self, val)
        class(vec_stack), intent(inout) :: self
        type(vec), intent(in) :: val
        self%ptr = self%ptr + 1
        if (self%ptr >= size(self%vecs)) then
            stop 1
        else
            self%vecs(self%ptr) = val
        end if
    end subroutine push

    subroutine pop(self, val)
        class(vec_stack), intent(inout) :: self
        type(vec), intent(out) :: val
        if (self%ptr <= 0) then
            stop 1
        end if
        val = self%vecs(self%ptr)
        self%ptr = self%ptr - 1
    end subroutine pop

end module day16helper

program day16
    use iso_fortran_env, only: int64
    use day16helper
    implicit none

    character(len=256) :: cur_line ! Arbitrary but long enough
    character, allocatable :: grid(:, :)
    integer :: rows, cols, iostat, i, j
    integer :: part2_max
    integer(int64) :: begin, end, rate

    call system_clock(begin, rate)
    open(26, file='day16.txt', status='old')
    cols = 0
    rows = 0
    do while (.true.)
        read(26, '(A)', iostat=iostat) cur_line
        if (iostat /= 0) then
            exit
        end if
        rows = rows + 1
        if (cols == 0) then
            cols = len(trim(cur_line))
        end if
    end do
    rewind(26)
    allocate(grid(cols, rows))
    do i = 1, rows
        read(26, '(A)') cur_line
        do j = 1, cols
            grid(j, i) = cur_line(j:j)
        end do
    end do
    close(26)
    call system_clock(end)
    print *, "IO took ", end - begin, " cycles, where there are ", rate, "cycles/s"
    call system_clock(begin)

    print *, "Part 1: ", energized(grid, vec(pos(1, 1), right))
    call system_clock(end)
    print *, "Part 1 took ", end - begin, " cycles, where there are ", rate, "cycles/s"
    call system_clock(begin)

    part2_max = 0
    do i = 1, size(grid, 2)
        part2_max = max(part2_max, energized(grid, vec(pos(1, i), right)))
        part2_max = max(part2_max, energized(grid, vec(pos(size(grid, 1), i), left)))
    end do
    do j = 1, size(grid, 1)
        part2_max = max(part2_max, energized(grid, vec(pos(j, 1), down)))
        part2_max = max(part2_max, energized(grid, vec(pos(j, size(grid, 1)), up)))
    end do

    print *, "Part 2: ", part2_max
    call system_clock(end)
    print *, "Part 2 took ", end - begin, " cycles, where there are ", rate, "cycles/s"
end program day16