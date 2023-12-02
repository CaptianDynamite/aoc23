program day1
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
            temp = 0
            if ('0' < current .and. current <= '9') then
                read(current, '(I1)') temp
            elseif (input(j:(j+2)) == 'one') then
                temp = 1
            elseif (input(j:(j+2)) == 'two') then
                temp = 2
            elseif (input(j:(j+4)) == 'three') then
                temp = 3
            elseif (input(j:(j+3)) == 'four') then
                temp = 4
            elseif (input(j:(j+3)) == 'five') then
                temp = 5
            elseif (input(j:(j+2)) == 'six') then
                temp = 6
            elseif (input(j:(j+4)) == 'seven')  then
                temp = 7
            elseif (input(j:(j+4)) == 'eight') then
                temp = 8
            elseif (input(j:(j+3)) == 'nine') then
                temp = 9
            end if
            if (temp > 0) then
                running_sum = running_sum + (temp * 10)
                exit find_first
            end if
        end do find_first
        find_last: do j = len(input), 1, -1
            current = input(j:j)
            temp = 0
            if ('0' < current .and. current <= '9') then
                read(current, '(I1)') temp
            elseif (input(j:(j+2)) == 'one') then
                temp = 1
            elseif (input(j:(j+2)) == 'two') then
                temp = 2
            elseif (input(j:(j+4)) == 'three') then
                temp = 3
            elseif (input(j:(j+3)) == 'four') then
                temp = 4
            elseif (input(j:(j+3)) == 'five') then
                temp = 5
            elseif (input(j:(j+2)) == 'six') then
                temp = 6
            elseif (input(j:(j+4)) == 'seven')  then
                temp = 7
            elseif (input(j:(j+4)) == 'eight') then
                temp = 8
            elseif (input(j:(j+3)) == 'nine') then
                temp = 9
            end if
            if (temp > 0) then
                running_sum = running_sum + temp
                exit find_last
            end if
        end do find_last
    end do

    write(*, *) running_sum
end program day1
