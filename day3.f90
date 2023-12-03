module helper
    implicit none
    private

    public parse_next_num, is_digit, to_digit

contains

    function is_digit(char) result(ans)
        character, intent(in) :: char
        logical :: ans
        ans = ('0' <= char) .and. (char <= '9')
    end function

    function to_digit(char) result(num)
        character, intent(in) :: char
        integer :: num
        num = ichar(char) - ichar('0')
    end function

    function parse_next_num(string) result(num)
        character (len=*), intent(in) :: string
        integer :: num
        integer :: i
        num = 0
        do i = 1, len(string)
            if (is_digit(string(i:i))) then
                num = num * 10
                num = num + to_digit(string(i:i))
            else
                exit
            end if
        end do
    end function parse_next_num

end module helper

program day3
    use helper
    implicit none
    character(len=200) :: line(140), current, check1, check2, check3
    integer :: num_at_pos(140,200), prel(200), curl(200), nexl(200)
    integer :: i, j, k, num_end, cur_num
    integer :: part1, part2
    logical :: covered

    line = ''
    part1 = 0
    num_at_pos = 0

    open(1, file="day3.txt", status="old")
    read (1, '(A)') line
    close(1)

!    Part 1
    do i = 1, 140
        current = line(i)
        j = 1
        do while (j <= len(current))
            if (is_digit(current(j:j))) then
                cur_num = parse_next_num(current(j:))

                num_end = j
                do while (is_digit(current(num_end:num_end)))
                    num_at_pos(i, num_end) = cur_num
                    num_end = num_end + 1
                end do

                check1 = line( max(i - 1, 1) )( max(j - 1, 1):num_end )
                check2 = line(i)( max(j - 1, 1):num_end )
                check3 = line( min(i + 1, 140 ) )( max(j - 1, 1):num_end )

                covered = .false.
                do k = 1, len(check1)
                    if (.not. is_digit(check1(k:k)) .and. check1(k:k) /= '' .and. check1(k:k) /= '.') then
                        part1 = part1 + cur_num
                        covered = .true.
                        exit
                    end if
                end do
                if (.not. covered) then
                    do k = 1, len(check2)
                        if (.not. is_digit(check2(k:k)) .and. check2(k:k) /= '' .and. check2(k:k) /= '.') then
                            part1 = part1 + cur_num
                            covered = .true.
                            exit
                        end if
                    end do
                end if
                if (.not. covered) then
                    do k = 1, len(check3)
                        if (.not. is_digit(check3(k:k)) .and. check3(k:k) /= '' .and. check3(k:k) /= '.') then
                            part1 = part1 + cur_num
                            covered = .true.
                            exit
                        end if
                    end do
                end if

                j = num_end
            else
!                Do nothing
            end if
            j = j + 1
        end do
    end do
    print *, "Part 1: ", part1

    do i = 1, 140
        current = line(i)
        do j = 1, len(current)
            if (current(j:j) == '*') then
!                Look in the following pattern as each number has to be in the surrounding positions.
!                ...
!                .*.
!                ...
                prel = num_at_pos( max(i - 1, 1), : )
                curl = num_at_pos( i, : )
                nexl = num_at_pos( min(i + 1, 140 ), : )

                cur_num = 1
                k = 0
                if (.not. all(prel == curl)) then
                    if (prel( max(j - 1, 1) ) > 0) then
                        cur_num = cur_num * prel( max(j - 1, 1) )
                        k = k + 1
                        if (prel( j ) == 0 .and. prel( j + 1 ) > 0) then
                            cur_num = cur_num * prel( j + 1 )
                            k = k + 1
                        end if
                    elseif (prel(j) > 0 ) then
                        cur_num = cur_num * prel( j )
                        k = k + 1
                    elseif (prel(j + 1) > 0) then
                        cur_num = cur_num * prel(j + 1)
                        k = k + 1
                    end if
                end if

                if (curl( max(j - 1, 1) ) > 0) then
                    cur_num = cur_num * curl( max(j - 1, 1) )
                    k = k + 1
                end if
                if (curl( j + 1 ) > 0 ) then
                    cur_num = cur_num * curl( j + 1 )
                    k = k + 1
                end if

                if (.not. all(curl== nexl)) then
                    if (nexl( max(j - 1, 1) ) > 0) then
                        cur_num = cur_num * nexl( max(j - 1, 1) )
                        k = k + 1
                        if (nexl( j ) == 0 .and. nexl( j + 1 ) > 0) then
                            cur_num = cur_num * nexl( j + 1 )
                            k = k + 1
                        end if
                    elseif (nexl(j) > 0 ) then
                        cur_num = cur_num * nexl( j )
                        k = k + 1
                    elseif (nexl( j + 1 ) > 0) then
                        cur_num = cur_num * nexl( j + 1 )
                        k = k + 1
                    end if
                end if

                if (k == 2) then
                    part2 = part2 + cur_num
                end if
            end if
        end do
    end do
    print *, "Part 2: ", part2

end program day3