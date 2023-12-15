module day15helper
    use iso_fortran_env, only: int64
    implicit none

    type box
        character(len=8) :: names(2048) ! Arbitrary but big enough
        integer :: lens(2048)
        integer :: elements
    contains
        procedure :: add
        procedure :: remove
        procedure :: focus_power
    end type box

contains
    subroutine add(self, code, lens)
        class(box), intent(inout) :: self
        character(len=*), intent(in) :: code
        integer, intent(in) :: lens
        integer :: i
        do i = 1, self%elements
            if (code == trim(self%names(i))) then
                self%lens(i) = lens
                return
            end if
        end do
        i = self%elements + 1
        self%names(i) = code
        self%lens(i) = lens
        self%elements = self%elements + 1
    end subroutine add

    subroutine remove(self, code)
        class(box), intent(inout) :: self
        character(len=*), intent(in) :: code
        integer :: i
        do i = 1, self%elements
            if (code == trim(self%names(i))) then
                self%names(i:self%elements - 1) = self%names(i + 1:self%elements)
                self%lens(i:self%elements - 1) = self%lens(i + 1:self%elements)
                self%elements = self%elements - 1
                return
            end if
        end do
    end subroutine remove

    pure elemental function focus_power(self, index) result(power)
        class(box), intent(in) :: self
        integer, intent(in) :: index
        integer(int64) :: power
        integer :: i
        power = sum(index * [(i, i = 1, self%elements)] * self%lens(:self%elements))
    end function focus_power

    pure function index_of(str, char) result(index)
        character(len=*), intent(in) :: str
        character, intent(in) :: char
        integer :: index
        integer :: i
        index = 0
        do i = 1, len(str)
            if (str(i:i) == char) then
                index = i
                return
            end if
        end do
    end function index_of

    pure function str_occ(str, char) result(occ)
        character(len=*), intent(in) :: str
        character, intent(in) :: char
        integer :: occ
        integer :: i
        occ = 0
        do i = 1, len(str)
            if (str(i:i) == char) then
                occ = occ + 1
            end if
        end do
    end function str_occ

    pure function longest_substr(str, delim) result(length)
        character(len=*), intent(in) :: str
        character, intent(in) :: delim
        integer(int64) :: length
        integer :: i, cur_len
        length = 0
        cur_len = 0
        do i = 1, len(str)
            if (str(i:i) == delim) then
                length = max(cur_len, length)
                cur_len = 0
            else
                cur_len = cur_len + 1
            end if
        end do
    end function longest_substr

    pure function split(str, char) result(substr)
        character(len=*), intent(in) :: str
        character, intent(in) :: char
        character(len=longest_substr(str, char)) :: substr(str_occ(str, char) + 1)
        integer :: i, cur, start_index
        substr = ''
        cur = 0
        start_index = 1
        do i = 1, len(str)
            if (str(i:i) == char) then
                cur = cur + 1
                substr(cur)(1 : i - start_index) = str(start_index:i - 1)
                start_index = i + 1
            end if
        end do
        if (cur < size(substr)) then
            i = len(str)
            substr(cur + 1)(1 : i - start_index + 1) = str(start_index:i)
        end if
    end function split

    pure elemental function hash(str) result(val)
        character(len=*), intent(in) :: str
        integer(int64) :: val
        integer :: i
        val = 0
        do i = 1, len(str)
            if (str(i:i) == '') then
                exit
            end if
            val = mod((val + iachar(str(i:i))) * 17, 256)
        end do
    end function hash
end module day15helper

program day15
    use day15helper
    implicit none
    character(len=2**16) :: raw_input ! Arbitrary but big enough
    character(len=:), allocatable :: input
    character(len=:), allocatable :: specs(:)
    type(box) :: boxes(0:255), cur
    integer :: i, index, lens, h
    integer(int64) :: begin, end, rate

    call system_clock(begin, rate)
    open(25, file='day15.txt', status='old')
    read(25, '(A)') raw_input
    input = trim(raw_input)
    close(25)
    call system_clock(end)
    print *, "IO took: ", end - begin, " cycles, where there are ", rate, "cycles/s"

    call system_clock(begin)
    specs = split(input, ',')
    print *, "Part 1: ", sum(hash(specs))
    call system_clock(end)
    print *, "Part 1 took: ", end - begin, " cycles, where there are ", rate, "cycles/s"

    call system_clock(begin)
    boxes = box('', 0, 0)
    do i = 1, size(specs)
        index = index_of(specs(i), '-')
        if (index /= 0) then
            h = hash(specs(i)(:index - 1))
            call boxes(h)%remove( specs(i)(:index - 1) )
        end if
        index = index_of(specs(i), '=')
        if (index /= 0) then
            read( specs(i)(index + 1:), '(I1)' ) lens
            h = hash(specs(i)(:index - 1))
            call boxes(h)%add( specs(i)(:index - 1), lens)
        end if
    end do
    print *, 'Part 2: ', sum(boxes%focus_power([(i, i = 1, size(boxes))]))
    call system_clock(end)
    print *, "Part 2 took: ", end - begin, " cycles, where there are ", rate, "cycles/s"
end program day15