module day13helper
    implicit none
contains
    impure function row_reflect_loc(map, smudges) result(loc)
        character, intent(in) :: map(:, :)
        integer, intent(in) :: smudges
        integer :: loc
        integer :: fwd, back, i, m, mi, smudge_remain, diff
        integer :: reflect_length(size(map, 2))
        logical :: reflect
        loc = 0
        reflect_length = 0
        do i = 1, size(map, 2) - 1
            smudge_remain = smudges
            fwd = 1
            back = 0
            reflect = .true.
            inner: do while (i + back > 0 .and. i + fwd <= size(map, 2))
                diff = count(map(:, i + back) /= map(:, i + fwd))
                if (diff > smudge_remain) then
                    reflect = .false.
                    exit inner
                else
                    smudge_remain = smudge_remain - diff
                end if
                fwd = fwd + 1
                back = back - 1
            end do inner
            if (reflect .and. smudge_remain == 0) then
                reflect_length(i) = fwd
            end if
        end do
        m = 1
        mi = 0
        do i = 1, size(reflect_length)
            if (reflect_length(i) >= m) then
                m = reflect_length(i)
                mi = i
            end if
        end do
        loc = mi
    end function row_reflect_loc
end module day13helper

program day13
    use iso_fortran_env, only: int64
    use day13helper
    implicit none

    character(len=512) :: cur_line ! Arbitrary but large enough
    character(len=512) :: cur_map(512) ! Arbitrary but large enough
    character, allocatable :: map(:, :), tmap(:, :)
    integer :: iostat, lines, line_length, i, j, fwd, back, reflect, temp
    integer :: part1, part2
    integer(int64) :: begin, end, rate

    call system_clock(begin, rate)
    open(24, file='day13.txt', status='old')
    part1 = 0
    part2 = 0
    lines = 0
    line_length = 0
    do while (.true.)
        read(24, '(A)', iostat=iostat) cur_line
        if (line_length == 0) then
            line_length = len(trim(cur_line))
            cur_map = ''
        end if
        if (iostat /= 0 .or. cur_line == '') then
            allocate(map(line_length, lines))
            do i = 1, lines
                cur_line = cur_map(i)
                do j = 1, line_length
                    map(j, i) = cur_line(j:j)
                end do
            end do

            allocate(tmap(lines, line_length))
            tmap = transpose(map)

            temp = row_reflect_loc(map, 0)
            part1 = part1 + 100 * temp
            if (temp == 0) then
                part1 = part1 + row_reflect_loc(tmap, 0)
            end if

            temp = row_reflect_loc(map, 1)
            part2 = part2 + 100 * temp
            if (temp == 0) then
                part2 = part2 + row_reflect_loc(tmap, 1)
            end if

            line_length = 0
            lines = 0
            if (iostat /= 0) then
                exit
            end if
            deallocate(tmap)
            deallocate(map)
        else
            lines = lines + 1
            cur_map(lines) = cur_line
        end if
    end do
    close(24)

    print *, "Part 1: ", part1
    print *, "Part 2: ", part2
    call system_clock(end)
    print *, "Day 13 took: ", end - begin, " cycles, where there are ", rate, " cycles/s"

end program day13