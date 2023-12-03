module strings
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

end module strings

program day3
    use strings
    implicit none
    character(len=200) :: line(10), current, check1, check2, check3
    integer :: i, j, k, num_end, cur_num
    integer :: part1
    logical :: covered

    line = ''
    part1 = 0

    open(1, file="day3.txt", status="old")
    read (1, '(A)') line
    close(1)

    do i = 1, 10
        current = line(i)
        j = 1
        do while (j <= len(current))
            if (is_digit(current(j:j))) then
                cur_num = parse_next_num(current(j:))

                num_end = j
                do while (is_digit(current(num_end:num_end)))
                    num_end = num_end + 1
                end do

                check1 = line( max(i - 1, 1) )( max(j - 1, 1):num_end )
                check2 = line(i)( max(j - 1, 1):num_end )
                check3 = line( min(i + 1, 10 ) )( max(j - 1, 1):num_end )

                print *, "-----------------"
                print *, check1
                print *, check2
                print *, check3

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
                print *, covered

                j = num_end
            else
!                Do nothing
            end if
            j = j + 1
        end do
    end do
    print *, "Part 1: ", part1
end program day3