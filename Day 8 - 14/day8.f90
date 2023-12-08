module day8helper
    use iso_fortran_env, only: int64
    implicit none

    type node
        character(len=3) :: cur, left, right
    contains
        procedure :: next_pos
    end type node

contains
    function lcm(nums) result(res)
        integer(int64), intent(in) :: nums(:)
        integer(int64) :: res
        integer(int64) :: lcf
        integer :: i
        lcf = gcd(nums(1), nums(2))
        do i = 3, size(nums)
            lcf = gcd(lcf, nums(i))
        end do
        res = nums(1) * nums(2) / lcf
        do i = 3, size(nums)
            res = res * nums(i) / lcf
        end do
    end function lcm

    function gcd(num1, num2) result(res)
        integer(int64) , intent(in) :: num1, num2
        integer(int64)  :: a, b, temp
        integer(int64)  :: res
        a = max(num1, num2)
        b = min(num1, num2)
        do while (b /= 0)
            temp = mod(a, b)
            a = b
            b = temp
        end do
        res = a
    end function gcd

    pure elemental function next_pos(self, dir) result(next)
        class(node), intent(in) :: self
        character, intent(in) :: dir
        character(len=3) :: next
        if (dir == 'L') then
            next = self%left
        elseif(dir == 'R') then
            next = self%right
        end if
    end function next_pos

    pure elemental function str_to_node(str) result(new)
        character(len=*), intent(in) :: str
        type(node) :: new
        new%cur = str(1:3)
        new%left = str(8:11)
        new%right = str(13:16)
    end function str_to_node

    pure elemental function code_to_index(code) result(index)
        character(len=3), intent(in) :: code
        integer :: index
        index = 26 ** 2 * capital_to_num(code(1:1)) + 26 * capital_to_num(code(2:2)) + capital_to_num(code(3:3)) + 1
    end function code_to_index

    pure function capital_to_num(cap) result(num)
        character, intent(in) :: cap
        integer :: num
        num = iachar(cap) - iachar('A')
    end function capital_to_num
end module day8helper

program day8
    use iso_fortran_env, only: int64
    use day8helper
    implicit none
    character(len=512) :: lr_seq_buffer, lr_next ! 256 is arbitrary but longer than we need
    character(len=:), allocatable :: lr_seq
    character, allocatable :: lr_nexts(:)
    character(len=64) :: cur_line ! 64 is arbitrary but longer than any line we have
    type(node) :: map(26*26*26), temp, cur
    character(len=64) :: a_in_last(26*26*26) ! a_in_last is vastly larger than it needs to be, but this is the most it could possibly be
    type(node), allocatable :: simul(:)
    integer(int64) :: iostat, i, j, part1, ends_with_a
    integer, allocatable :: counts_between(:)
    logical, allocatable :: cycle_length_found(:)
    integer(int64) :: beginning, end, rate

    call system_clock(beginning, rate)

    open(18, file='day8.txt', status='old')
    read(18, '(A)') lr_seq_buffer
    lr_seq = trim(lr_seq_buffer)
    allocate(lr_nexts(len(lr_seq)))
    read(18, '(A)') ! Blank line
    iostat = 0

    ends_with_a = 0
    map = node('', '', '')
    do while (iostat == 0)
        read(18, '(A)', iostat=iostat) cur_line
        temp = str_to_node(trim(cur_line))
        map(code_to_index(temp%cur)) = temp
        if (cur_line(3:3) == 'A') then
            ends_with_a = ends_with_a + 1
            a_in_last(ends_with_a) = cur_line
        end if
    end do
    simul = str_to_node(a_in_last(1:ends_with_a))
    close(18)

    call system_clock(end)
    print *, "Data reading took ", end - beginning, " cycles, where there is ", rate, " cycles/sec"
    call system_clock(beginning, rate)

    i = 0
    part1 = 0
    cur = map(1)
    do while (cur%cur /= 'ZZZ')
        part1 = part1 + 1
        i = i + 1
        if (i > len(lr_seq)) then
            i = 1
        end if
        lr_next = lr_seq(i:i)
        cur = map(code_to_index(cur%next_pos(lr_next)))
    end do
    print *, "Part 1: ", part1

    call system_clock(end)
    print *, "Part 1 took ", end - beginning, " cycles, where there is ", rate, " cycles/sec"
    call system_clock(beginning, rate)

    i = 0
    allocate(counts_between(size(simul)))
    allocate(cycle_length_found(size(counts_between)))
    counts_between = 0
    cycle_length_found = .false.
    do while(any(.not. cycle_length_found))
        do j = 1, size(simul)
            if (.not. cycle_length_found(j) .and. simul(j)%cur(3:3) == 'Z') then
                cycle_length_found(j) = .true.
            elseif (.not. cycle_length_found(j)) then
                counts_between(j) = counts_between(j) + 1
            end if
        end do
        i = i + 1
        if (i > len(lr_seq)) then
            i = 1
        end if
        lr_nexts = lr_seq(i:i)
        simul = map(code_to_index(simul%next_pos(lr_nexts)))
    end do
    print *, "Part 2: ", lcm(int8(counts_between))

    call system_clock(end)
    print *, "Part 2 took ", end - beginning, " cycles, where there is ", rate, " cycles/sec"
    call system_clock(beginning, rate)
end program day8