module day14helper
    use iso_fortran_env, only: int64
    implicit none

    enum, bind(C)
        enumerator :: empty = 0
        enumerator :: round = 1
        enumerator :: cube  = 2
    end enum

    type pos
        integer :: row, col
    end type pos
contains
    subroutine print_grid(grid)
        integer, intent(in) :: grid(:, :)
        integer :: i, j
        do i = 1, size(grid, 2)
            do j = 1, size(grid, 1)
                if (grid(j,i) == cube) then
                    write(*, '(A1)', advance='no') '#'
                elseif (grid(j,i) == round) then
                    write(*, '(A1)', advance='no') 'O'
                else
                    write(*, '(A1)', advance='no') '.'
                end if
            end do
            write (*, *)
        end do
    end subroutine print_grid

    pure function weight(grid) result(w)
        integer, intent(in) :: grid(:, :)
        integer :: w
        integer :: grid_copy(size(grid, 1), size(grid, 2))
        integer :: cost(size(grid, 2))
        integer :: i
        grid_copy = grid
        where (grid_copy == cube)
            grid_copy = empty
        end where
        cost = [(i, i = size(grid, 2), 1, -1)]
        w = sum(matmul(grid_copy, cost))
    end function weight
end module day14helper

program day14
    use iso_fortran_env, only: int64
    use day14helper
    implicit none

    character(len=512) :: cur_line ! Longer than we need
    integer, allocatable :: p1_grid(:, :), p2_grid(:, :)
    integer :: weight_hist(1000) ! Longer than we need
    integer :: iostat, cols, rows, i, j, k, cycle, length, cyclic_len, cyclic_offset
    integer(int64) :: begin, end, rate

    call system_clock(begin, rate)
    open(24, file='day14.txt', status='old')
    cols = 0
    rows = 0
    do while (.true.)
        read(24, '(A)', iostat=iostat) cur_line
        if (cols == 0) then
            cols = len(trim(cur_line))
        end if
        if (iostat /= 0) then
            exit
        end if
        rows = rows + 1
    end do
    allocate(p1_grid(cols, rows))

    rewind(24)
    do i = 1, rows
        read(24, '(A)', iostat=iostat) cur_line
        do j = 1, cols
            if (cur_line(j:j) == '#') then
                p1_grid(j, i) = cube
            elseif (cur_line(j:j) == 'O') then
                p1_grid(j, i) = round
            else
                p1_grid(j, i) = empty
            end if
        end do
    end do
    close(24)
    call system_clock(end)
    print *, "Data reading took: ", end - begin, " cycles. Where there are ", rate, "cycles/s"
    call system_clock(begin)

    p2_grid = p1_grid

    do i = 2, rows
        do k = 2, rows - (i - 2)
            do j = 1, cols
                if (p1_grid(j, k) == round .and. p1_grid(j, k - 1) == empty) then
                    p1_grid(j, k - 1) = p1_grid(j, k)
                    p1_grid(j, k) = empty
                end if
            end do
        end do
    end do
    print *, 'Part 1: ', weight(p1_grid)

    call system_clock(end)
    print *, "Part 1 took: ", end - begin, " cycles. Where there are ", rate, "cycles/s"
    call system_clock(begin)

    outer: do cycle = 1, size(weight_hist)
        do i = 2, rows
            do k = 2, rows - (i - 2)
                do j = 1, cols
                    if (p2_grid(j, k) == round .and. p2_grid(j, k - 1) == empty) then
                        p2_grid(j, k - 1) = p2_grid(j, k)
                        p2_grid(j, k) = empty
                    end if
                end do
            end do
        end do
        do j = 2, cols
            do k = 2, cols - (j - 2)
                do i = 1, rows
                    if (p2_grid(k, i) == round .and. p2_grid(k - 1, i) == empty) then
                        p2_grid(k - 1, i) = p2_grid(k, i)
                        p2_grid(k, i) = empty
                    end if
                end do
            end do
        end do
        do i = rows - 1, 1, -1
            do k = rows - 1, 1 + (rows - i - 1), -1
                do j = 1, cols
                    if (p2_grid(j, k) == round .and. p2_grid(j, k + 1) == empty) then
                        p2_grid(j, k + 1) = p2_grid(j, k)
                        p2_grid(j, k) = empty
                    end if
                end do
            end do
        end do
        do j = cols - 1, 1, -1
            do k = cols - 1,  1 + (cols - j - 1), -1
                do i = 1, rows
                    if (p2_grid(k, i) == round .and. p2_grid(k + 1, i) == empty) then
                        p2_grid(k + 1, i) = p2_grid(k, i)
                        p2_grid(k, i) = empty
                    end if
                end do
            end do
        end do
        weight_hist(cycle) = weight(p2_grid)
        do length = 2, cycle / 2
            if (all( weight_hist(cycle - length + 1 : cycle) == weight_hist(cycle - 2 * length + 1 : cycle - length) )) then
                cyclic_len = length
                cyclic_offset = cycle - length + 1
                exit outer
            end if
        end do
    end do outer

    print *, "Part 2: ", weight_hist(cyclic_offset + mod(1000000000 - cyclic_offset, cyclic_len))

    call system_clock(end)
    print *, "Part 2 took: ", end - begin, " cycles. Where there are ", rate, "cycles/s"

end program day14