module day7helper
    use iso_fortran_env, only: int64
    implicit none

    type :: hand
        character(len=5) :: hand
        integer(int64) :: bid
    contains
        procedure :: compare
        procedure :: hand_type
        procedure :: equal
    end type hand

    enum, bind(c) !TODO find out why bind(c) is required
        enumerator :: high = 1
        enumerator :: one = 2
        enumerator :: two = 3
        enumerator :: three = 4
        enumerator :: full = 5
        enumerator :: four = 6
        enumerator :: five = 7
    endenum

contains
    subroutine sort_hands(hands)
        type(hand), intent(inout) :: hands(:)
        type(hand) :: copy_hands(size(hands))
        integer :: i, j, comp
        type(hand) :: cur
        logical :: placed
        do i = 1, size(hands)
            cur = hands(i)
            placed = .false.
            do j = 1, i-1
                if (.not. copy_hands(j)%equal(cur)) then
                    comp = cur%compare(copy_hands(j))
                    if (comp <= 0) then
                        copy_hands(j + 1:i) = copy_hands(j:i-1)
                        copy_hands(j) = cur
                        placed = .true.
                        exit
                    end if
                end if
            end do
            if (.not. placed) then
                copy_hands(i) = cur
            end if
        end do
        hands = copy_hands
    end subroutine

    function compare(self, to) result(equal)
        class(hand), intent(in) :: self
        class(hand), intent(in) :: to
        integer :: equal
        if (self%hand_type() == to%hand_type()) then
            if (val_compare(self%hand, to%hand) == -1) then
                equal = -1
            else if (val_compare(self%hand, to%hand) == 0) then
                equal = 0
            else
                equal = 1
            end if
        elseif (self%hand_type() < to%hand_type()) then
            equal = -1
        else
            equal = 1
        end if
    end function compare

    pure function val_compare(vals1, vals2) result(val)
        character(len=*), intent(in) :: vals1, vals2
        integer :: val
        integer :: i, j, k
        character :: ordering(13)
        ordering = ['A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2']
        outer: do i = 1, len(vals1)
            next: do j = 1, size(ordering)
                do k = 1, size(ordering)
                    if (vals1(i:i) == ordering(j) .and. vals2(i:i) == ordering(k)) then
                        if (j < k) then
                            val = 1
                            exit outer
                        elseif(j == k) then
                            val = 0
                            cycle next
                        else
                            val = -1
                            exit outer
                        end if
                    end if
                end do
            end do next
        end do outer
    end function val_compare

    function hand_type(self) result(type)
        class(hand), intent(in) :: self
        integer :: type
        integer :: seen, count(len(self%hand))
        integer :: i, j
        logical :: found
        character :: cur, seen_order(len(self%hand))
        count = 0
        seen = 0
        seen_order = '\0'
        do i = 1, len(self%hand)
            cur = self%hand(i:i)
            found = .false.
            do j = 1, seen
                if (cur == seen_order(j)) then
                    count(j) = count(j) + 1
                    found = .true.
                    exit
                end if
            end do
            if (.not. found) then
                seen = seen + 1
                seen_order(seen) = cur
                count(seen) = 1
            end if
        end do

        if (seen == 1) then
            type = five
        elseif (seen == 2) then
            if (any(count == 4)) then
                type = four
            else
                type = full
            end if
        elseif (seen == 3) then
            if (any(count == 3)) then
                type = three
            else
                type = two
            end if
        elseif (seen == 4) then
            type = one
        else
            type = high
        end if
    end function hand_type

    function equal(h1, h2) result(eq)
        class(hand), intent(in) :: h1, h2
        logical :: eq
        eq = h1%hand == h2%hand .and. h1%bid == h2%bid
    end function equal

end module day7helper

program day7
    use day7helper
    implicit none
    type(hand), allocatable :: hands(:)
    integer :: iostat, i, hand_count
    integer :: part1

    open(17, file='day7.txt', status='old')
    iostat = 0
    hand_count = 0
    do while (iostat == 0)
        read(17, '(A)', iostat=iostat)
        hand_count = hand_count + 1
    end do

    allocate(hands(hand_count))
    rewind(17)
    read (17, *, iostat=iostat) hands
    close(17)

    call sort_hands(hands)
    print '(A I4)', hands
    part1 = 0
    do i = 1, size(hands)
        part1 = part1 + i * hands(i)%bid
    end do
    print *, "Part 1: ", part1

end program day7