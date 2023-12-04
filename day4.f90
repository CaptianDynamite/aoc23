module day4helper
    implicit none
    private
    public split, char_count
contains
    function char_count(string, char) result(count)
        character (len=*), intent(in) :: string
        character, intent(in) :: char
        integer :: count, i
        count = 0
        do i = 1, len(string)
            if (string(i:i) == char) then
                count = count + 1
            end if
        end do
    end function char_count

    function split(string, split_on) result(parts)
        character (len=*), intent(in) :: string
        character, intent(in) :: split_on
        character (len=:), allocatable :: parts(:)
        integer :: i, split_i
        integer :: start_index

        allocate(character (len=len(string)) :: parts(char_count(string, split_on) + 1))

        split_i = 1
        start_index = 1

        do i = 1, len(string)
            if (string(i:i) == split_on) then
                if (i == start_index) then
                    start_index = start_index + 1
                    cycle
                end if
                parts(split_i) = string(start_index : i - 1)
                start_index = i + 1
                split_i = split_i + 1
            end if
        end do
        parts(split_i) = string(start_index : i)
    end function split
end module day4helper

program day4
    use day4helper
    implicit none

    character (len=200) :: lines(199), curline, card_name, winning_nums, numbers
    character (len=:), allocatable :: split_parts(:)
    integer :: i, j
    integer :: card_nums (199), winning(10,199), nums(25, 199), score(199), part2(199)
    integer :: part1, part1_doubler

    open(1, file='day4.txt', status='old')
    read(1, '(A)') lines

    part1 = 0
    score = 0
    part2 = 1

    do i = 1, size(lines)
        curline = lines(i)
        split_parts = split(curline, ':')
        card_name = split_parts(1)
        curline = split_parts(2)

        split_parts = split(curline, '|')
        winning_nums = split_parts(1)
        numbers = split_parts(2)

        split_parts = split(card_name, ' ')
        read (split_parts(2), '(I5)') card_nums(i)

        split_parts = split(winning_nums, ' ')
        do j = 1, size(winning(:, i))
            read (split_parts(j), '(I5)') winning(j, i)
        end do

        split_parts = split(numbers, ' ')
        part1_doubler = 0
        do j = 1, size(nums(:, i))
            read (split_parts(j), '(I5)') nums(j, i)
            if (any(winning(:, i) == nums(j, i))) then
                if (part1_doubler == 0) then
                    part1_doubler = 1
                else
                    part1_doubler = part1_doubler * 2
                end if
                score(i) = score(i) + 1
            end if
        end do
        part1 = part1 + part1_doubler
        do j = 1, score(i)
            part2(i + j) = part2(i + j) + part2(i)
        end do
    end do

    print *, 'Part 1: ', part1
    print *, 'Part 2: ', sum(part2)
end program day4