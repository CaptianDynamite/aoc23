module day11helper
    implicit none
    type position
        integer x, y
    contains
        procedure :: taxicab_dist
    end type position
contains
    pure function taxicab_dist(self, to) result(dist)
        class(position), intent(in) :: self, to
        integer :: dist
        dist = abs(to%x - self%x) + abs(to%y - self%y)
    end function taxicab_dist

    pure function str_element_cmp(str, char) result(eq)
        character(len=*), intent(in) :: str
        character, intent(in) :: char
        logical :: eq(len(str))
        integer :: i
        do i = 1, len(str)
            eq(i) = str(i:i) == char
        end do
    end function str_element_cmp

end module day11helper

program day11
    use iso_fortran_env, only: int64
    use day11helper
    implicit none
    character(len=512) :: cur_line
    logical, allocatable :: column_empty(:), row_empty(:)
    type(position), allocatable :: galaxies(:)
    type(position), allocatable :: galaxies_pt2(:)
    integer :: iostat, i, j, lines, cur_x, cur_y, cx2, cy2, gal_count, cur_gal
    integer :: part1
    integer(int64) :: part2
    integer(int64) :: begin, end, rate

    call system_clock(begin, rate)
    open(21, file='day11.txt', status='old')
    iostat = 0
    lines = 0
    do while (iostat == 0)
        read(21, '(A)', iostat=iostat) cur_line
        if (iostat /= 0) then
            exit
        end if
        lines = lines + 1
    end do

    allocate(column_empty(len(trim(cur_line))))
    column_empty(:) = .true.
    allocate(row_empty(lines))
    rewind(21)
    gal_count = 0
    do i = 1, lines
        read(21, '(A)', iostat=iostat) cur_line
        column_empty = column_empty .and. str_element_cmp(trim(cur_line), '.')
        row_empty(i) = all(str_element_cmp(trim(cur_line), '.'))
        gal_count = gal_count + count(str_element_cmp(trim(cur_line), '#'))
    end do
    allocate(galaxies(gal_count))
    allocate(galaxies_pt2(gal_count))

    rewind(21)
    cur_y = 0
    cy2 = 0
    cur_gal = 0
    do i = 1, lines
        read(21, '(A)', iostat=iostat) cur_line
        cur_x = 0
        cx2 = 0
        cur_y = cur_y + 1
        cy2 = cy2 + 1
        if (row_empty(i)) then
            cur_y = cur_y + 1
            cy2 = cy2 + 999999
            cycle
        end if
        do j = 1, len(trim(cur_line))
            cur_x = cur_x + 1
            cx2 = cx2 + 1
            if (column_empty(j)) then
                cur_x = cur_x + 1
                cx2 = cx2 + 999999
                cycle
            end if
            if (cur_line(j:j) == '#') then
                cur_gal = cur_gal + 1
                galaxies(cur_gal) = position(cur_x, cur_y)
                galaxies_pt2(cur_gal) = position(cx2, cy2)
            end if
        end do
    end do
    close(21)

    part1 = 0
    part2 = 0
    do i = 1, gal_count
        do j = i + 1, gal_count
            part1 = part1 + galaxies(i)%taxicab_dist(galaxies(j))
            part2 = part2 + galaxies_pt2(i)%taxicab_dist(galaxies_pt2(j))
        end do
    end do

    print *, "Part 1: ", part1
    print *, "Part 2: ", part2

    call system_clock(end)
    print *, "Program took ", end - begin, " cycles, where there are ", rate, " cycles/s"
end program day11