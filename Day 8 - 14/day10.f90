module day10helper
    implicit none

    enum, bind(C)
        enumerator :: no_direction = 0
        enumerator :: from_north = 1
        enumerator :: from_south = 2
        enumerator :: from_east = 3
        enumerator :: from_west = 4
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
    implicit none

    character(len=512) :: cur_line
    type(node), allocatable :: grid(:, :)
    type(position) :: start_pos
    integer :: line_no, iostat, i, j, seen
    integer :: cur_direction(2)
    type(position) :: cur_position(2), temp_pos
    integer :: path_length(2)
    type(node) :: temp, start_node

    open(20, file='day10.txt', status='old')
    iostat = 0
    line_no = 0
    do while (iostat == 0)
        read(20, '(A)', iostat=iostat) cur_line
        line_no = line_no + 1
    end do
    allocate(grid(len(trim(cur_line)), line_no))
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
        cur_direction(seen) = from_east
        cur_position(seen) = temp_pos
    end if
    temp_pos = start_node%west
    temp = get_node(grid, temp_pos)
    if (.not. position_equal(temp%east, position(-1, -1))) then
        seen = seen + 1
        cur_direction(seen) = from_west
        cur_position(seen) = temp_pos
    end if

    path_length = 1

    do while (.not. any(position_equal(cur_position, start_pos)))
        do i = 1, size(cur_position)
            if (position_equal(cur_position(i), position(-1, -1))) then
                cycle
            end if
            path_length(i) = path_length(i) + 1
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
            else
                cur_position(i) = position(-1, -1)
                path_length(i) = 0
            end if
        end do
    end do
    print *, "Part 1: ",  maxval(path_length / 2)

end program day10