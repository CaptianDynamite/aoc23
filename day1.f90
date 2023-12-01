program aoc23
    implicit none
    character(len = 100):: input
    character :: current
    integer :: i, j
    integer :: running_sum, temp
    logical :: tens
    running_sum = 0
    open(1, file = 'day1.txt', status = 'old')
    do i = 1, 1000
        read(1, '(A)') input
        tens = .true.
        find_first: do j = 1, len(input)
            current = input(j:j)
            if ('0' <= current .and. current <= '9') then
                read(current, '(I1)') temp
                running_sum = running_sum + (temp * 10)
                exit find_first
            end if
        end do find_first
        find_last: do j = len(input), 1, -1
            current = input(j:j)
            if ('0' <= current .and. current <= '9') then
                read(current, '(I1)') temp
                running_sum = running_sum + temp
                exit find_last
            end if
        end do find_last
    end do

    write(*, *) running_sum
end program
