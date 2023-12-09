module day9helper
    implicit none

contains
    pure function string_occurences(string, char) result(count)
        character(len=*), intent(in) :: string
        character, intent(in) :: char
        integer :: count
        integer :: i
        count = 0
        do i = 1, len(string)
            if (string(i:i) == char) then
                count = count + 1
            end if
        end do
    end function string_occurences
end module day9helper

program day9
    use iso_fortran_env, only: int64
    use day9helper
    implicit none
    character(len=512) :: cur_line
    integer, allocatable :: seq(:), num_derivs(:, :)
    integer :: i, j, k, iostat, line_count, part1, part2

    open(19, file='day9.txt', status='old')
    part1 = 0
    part2 = 0
    iostat = 0
    do while (iostat == 0)
        read(19, '(A)', iostat=iostat) cur_line
        if (iostat /= 0) then
            cycle
        end if
        allocate(seq(string_occurences(trim(cur_line), ' ') + 1))
        read(cur_line, *) seq
        allocate(num_derivs(size(seq) + 2, 128))
        num_derivs = 0
        j = 1
        num_derivs(2:size(seq) + 1, j) = seq
        do while (sum(num_derivs(2:, j)) /= 0)
            num_derivs(j+2:size(seq)+1, j+1) = num_derivs(j+2:size(seq)+1, j) - num_derivs(j+1:size(seq), j)
            j = j + 1
        end do
        do k = j-1, 1, -1
            num_derivs(size(seq) + 2, k) = num_derivs(size(seq) + 1, k) + num_derivs(size(seq) + 2, k + 1)
            num_derivs(k, k) = num_derivs(k + 1, k) - num_derivs(k+1, k+1)
        end do
        part1 = part1 + num_derivs(size(seq) + 2, 1)
        part2 = part2 + num_derivs(1, 1)
        deallocate(num_derivs)
        deallocate(seq)
    end do
    print *, "Part 1: ", part1
    print *, "Part 2: ", part2
    close(19)

end program day9