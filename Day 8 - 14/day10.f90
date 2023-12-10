module day10helper
    implicit none

    enum, bind(C)
        enumerator :: no_direction = 0
        enumerator :: from_north = 1
        enumerator :: from_south = 2
        enumerator :: from_east = 3
        enumerator :: from_west = 4
    end enum

    enum, bind(C)
        enumerator :: outside = 0
        enumerator :: inside = 1
        enumerator :: pipe = 2
    end enum

    type position
        integer :: x, y
    end type position

    type node
        type(position) :: north, south, east, west
    end type node
contains
    pure function create_node(char, pos, max_pos) result(new)
        character, intent(in) :: char
        type(position), intent(in) :: pos, max_pos
        type(node) :: new
        type(position) :: n, s, e, w, null
        n = position(pos%x, pos%y - 1)
        s = position(pos%x, pos%y + 1)
        w = position(pos%x - 1, pos%y)
        e = position(pos%x + 1, pos%y)
        null = position(-1, -1)
        if (pos%y == 1) then
            n = null
        elseif (pos%y == max_pos%y) then
            s = null
        elseif (pos%x == 1) then
            w = null
        elseif (pos%x == max_pos%x) then
            e = null
        end if
        if (char == '|') then
            new = node(n, s, null, null)
        elseif (char == '-') then
            new = node(null, null, e, w)
        elseif (char == 'L') then
            new = node(n, null, e, null)
        elseif (char == 'J') then
            new = node(n, null, null, w)
        elseif (char == '7') then
            new = node(null, s, null, w)
        elseif (char == 'F') then
            new = node(null, s, e, null)
        elseif (char == '.') then
            new = node(null, null, null, null)
        elseif (char == 'S') then
            new = node(n, s, e, w)
        end if
    end function create_node

    pure function get_node(grid, pos) result(next)
        type(node), intent(in) :: grid(:, :)
        type(position), intent(in) :: pos
        type(node) :: next
        if (pos%x == -1 .or. pos%y == -1) then
            next = node(position(-1, -1), position(-1, -1), position(-1, -1), position(-1, -1))
        end if
        next = grid(pos%x, pos%y)
    end function get_node

    pure elemental function position_equal(pos1, pos2) result(equal)
        type(position), intent(in) :: pos1, pos2
        logical :: equal
        equal = pos1%x == pos2%x .and. pos1%y == pos2%y
    end function position_equal

    pure function not_null_direction(n, dir) result(out_dir)
        type(node), intent(in) :: n
        integer, intent(in) :: dir
        integer :: out_dir
        out_dir = no_direction
        if (dir == from_north) then
            if (.not. position_equal(n%south, position(-1, -1))) then
                out_dir = from_north
            elseif (.not. position_equal(n%east, position(-1, -1))) then
                out_dir = from_west
            elseif (.not. position_equal(n%west, position(-1, -1))) then
                out_dir = from_east
            end if
        elseif (dir == from_south) then
            if (.not. position_equal(n%north, position(-1, -1))) then
                out_dir = from_south
            elseif (.not. position_equal(n%east, position(-1, -1))) then
                out_dir = from_west
            elseif (.not. position_equal(n%west, position(-1, -1))) then
                out_dir = from_east
            end if
        elseif (dir == from_east) then
            if (.not. position_equal(n%north, position(-1, -1))) then
                out_dir = from_south
            elseif (.not. position_equal(n%south, position(-1, -1))) then
                out_dir = from_north
            elseif (.not. position_equal(n%west, position(-1, -1))) then
                out_dir = from_east
            end if
        elseif (dir == from_west) then
            if (.not. position_equal(n%north, position(-1, -1))) then
                out_dir = from_south
            elseif (.not. position_equal(n%south, position(-1, -1))) then
                out_dir = from_north
            elseif (.not. position_equal(n%east, position(-1, -1))) then
                out_dir = from_west
            end if
        end if
    end function not_null_direction
end module day10helper

program day10
    use day10helper
    use iso_fortran_env, only: int64
    implicit none

    character(len=512) :: cur_line
    type(node), allocatable :: grid(:, :)
    integer, allocatable :: part2_grid(:, :)
    type(position) :: start_pos
    integer :: line_no, iostat, i, j, seen, part2_odd_even, part2
    integer :: cur_direction(2)
    type(position) :: cur_position(2), temp_pos
    integer :: path_length(2)
    type(node) :: temp, start_node
    character :: start_pos_equiv
    integer(int64) :: begin, end, rate

    call system_clock(begin, rate)
    open(20, file='day10.txt', status='old')
    iostat = 0
    line_no = 0
    do while (iostat == 0)
        read(20, '(A)', iostat=iostat) cur_line
        line_no = line_no + 1
    end do
    allocate(grid(len(trim(cur_line)), line_no))
    allocate(part2_grid(len(trim(cur_line)), line_no))
    part2_grid = outside
    rewind(20)
    do i = 1, line_no
        read(20, '(A)', iostat=iostat) cur_line
        do j = 1, len(trim(cur_line))
            if (cur_line(j:j) == 'S') then
                start_pos = position(j, i)
            end if
            grid(j, i) = create_node(cur_line(j:j), position(j, i), position(size(grid(:, i)), size(grid(j, :))))
        end do
    end do
    close(20)
    call system_clock(end)
    print *, "File processing took: ", end - begin, " cycles. Where there is ", rate, " cycles/s"

    call system_clock(begin)
    seen = 0
    start_node = get_node(grid, start_pos)

    temp_pos = start_node%north
    temp = get_node(grid, temp_pos)
    if (.not. position_equal(temp%south, position(-1, -1))) then
        seen = seen + 1
        cur_direction(seen) = from_south
        cur_position(seen) = temp_pos
    end if
    temp_pos = start_node%south
    temp = get_node(grid, temp_pos)
    if (.not. position_equal(temp%north, position(-1, -1))) then
        seen = seen + 1
        cur_direction(seen) = from_north
        cur_position(seen) = temp_pos
    end if
    temp_pos = start_node%east
    temp = get_node(grid, temp_pos)
    if (.not. position_equal(temp%west, position(-1, -1))) then
        seen = seen + 1
        cur_direction(seen) = from_west
        cur_position(seen) = temp_pos
    end if
    temp_pos = start_node%west
    temp = get_node(grid, temp_pos)
    if (.not. position_equal(temp%east, position(-1, -1))) then
        seen = seen + 1
        cur_direction(seen) = from_east
        cur_position(seen) = temp_pos
    end if

    if (all(cur_direction == [from_south, from_north])) then
        start_pos_equiv = '|'
    elseif (all(cur_direction == [from_north, from_east])) then
        start_pos_equiv = '7'
    elseif (all(cur_direction == [from_north, from_west])) then
        start_pos_equiv = 'F'
    elseif (all(cur_direction == [from_south, from_east])) then
        start_pos_equiv = 'J'
    elseif (all(cur_direction == [from_south, from_west])) then
        start_pos_equiv = 'L'
    elseif (all(cur_direction == [from_west, from_east])) then
        start_pos_equiv = '-'
    end if

    grid(start_pos%x, start_pos%y) = create_node(start_pos_equiv, start_pos, position(size(grid(:, i)), size(grid(j, :))))

    part2_grid(start_pos%x, start_pos%y) = pipe
    part2_grid(cur_position(1)%x, cur_position(1)%y) = pipe
    part2_grid(cur_position(2)%x, cur_position(2)%y) = pipe

    path_length = 1
    do while (.not. any(position_equal(cur_position, start_pos)))
        path_length = path_length + 1
        do i = 1, size(cur_position)
            temp = get_node(grid, cur_position(i))
            cur_direction(i) = not_null_direction(temp, cur_direction(i))
            if (cur_direction(i) == from_north) then
                cur_position(i) = temp%south
            elseif (cur_direction(i) == from_south) then
                cur_position(i) = temp%north
            elseif (cur_direction(i) == from_east) then
                cur_position(i) = temp%west
            elseif (cur_direction(i) == from_west) then
                cur_position(i) = temp%east
            end if

            part2_grid(cur_position(i)%x, cur_position(i)%y) = pipe
        end do
    end do
    print *, "Part 1: ",  maxval(path_length / 2)
    call system_clock(end)
    print *, "Part 1 took: ", end - begin, " cycles. Where there is ", rate, " cycles/s"

    call system_clock(begin)
    part2 = 0
    do i = 1, size(part2_grid(1, :))
        part2_odd_even = 0
        do j = 1, size(part2_grid(:, 1))
            if (part2_grid(j, i) == pipe) then
                temp = grid(j, i)
                if (.not. position_equal(temp%north, position(-1, -1))) then
                    part2_odd_even = part2_odd_even + 1
                end if
            else
                if (mod(part2_odd_even, 2) == 1) then
                    part2 = part2 + 1
                end if
            end if
        end do
    end do

    where (part2_grid == pipe)
        part2_grid = outside
    end where

    print *, "Part 2: ", part2
    call system_clock(end)
    print *, "Part 2 took: ", end - begin, " cycles. Where there is ", rate, " cycles/s"

end program day10