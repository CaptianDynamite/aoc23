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
    use day11helper
    implicit none
    character(len=512) :: cur_line
    logical, allocatable :: column_empty(:), row_empty(:)
    type(position), allocatable :: galaxies(:)
    integer :: iostat, i, j, lines, cur_x, cur_y, gal_count, cur_gal
    integer :: part1

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
    rewind(21)
    allocate(galaxies(gal_count))
    cur_y = 0
    cur_gal = 0
    do i = 1, lines
        read(21, '(A)', iostat=iostat) cur_line
        cur_x = 0
        cur_y = cur_y + 1
        if (row_empty(i)) then
            cur_y = cur_y + 1
            cycle
        end if
        do j = 1, len(trim(cur_line))
            cur_x = cur_x + 1
            if (column_empty(j)) then
                cur_x = cur_x + 1
                cycle
            end if
            if (cur_line(j:j) == '#') then
                cur_gal = cur_gal + 1
                galaxies(cur_gal) = position(cur_x, cur_y)
            end if
        end do
    end do
    close(21)

    part1 = 0
    do i = 1, gal_count
        do j = i + 1, gal_count
            part1 = part1 + galaxies(i)%taxicab_dist(galaxies(j))
        end do
    end do

    print *, "Part 1: ", part1

end program day11