module utils
    implicit none
    private
    public index_of, index_of_skip, occurs_first

contains
    function index_of(string, character) result(index)
        character (len=*), intent(in) :: string
        character, intent(in) :: character
        integer :: index
        logical :: found
        found = .false.
        do index = 1, len(string)
            if (string(index:index) == character) then
                found = .true.
                exit
            end if
        end do
        if (.not. found) then
            index = 0
        end if
    end function

    function occurs_first(string, character1, character2) result(which)
        character (len=*), intent(in) :: string
        character, intent(in) :: character1, character2
        character :: which
        integer :: index
        which = '\0'
        do index = 1, len(string)
            if (string(index:index) == character1) then
                which = character1
                exit
            elseif (string(index:index) == character2) then
                which = character2
                exit
            end if
        end do
    end function

    function index_of_skip(string, character, skip) result(index)
        character (len=*), intent(in) :: string
        character, intent(in) :: character
        integer, intent(in) :: skip
        integer :: index, count
        logical :: found
        found = .false.
        count = 0
        do index = 1, len(string)
            if (string(index:index) == character) then
                if (count == skip) then
                    found = .true.
                    exit
                end if
                count = count + 1
            end if
        end do
        if (.not. found) then
            index = 0
        end if
    end function

end module utils

program day2
    use utils
    implicit none

    character (len=256) :: current_line
    integer :: i, id
    integer :: id_sum
    integer :: max_r(100), max_g(100), max_b(100)
    integer :: cur
    character :: colour_code

    max_r = 0
    max_g = 0
    max_b = 0
    id_sum = 0

    open (1, file='day2.txt', status='old')
    do i = 1, 100
        read (1, '(A)') current_line
        read (current_line(index_of(current_line, ' ') + 1:index_of(current_line, ':') - 1), "(I5)") id

        current_line = current_line(index_of(current_line, ':') + 1:)
!        print *, id

        do while (.true.)
            read(current_line(index_of(current_line, ' ') + 1:index_of_skip(current_line, ' ', 1)), "(I6)") cur
            current_line = current_line(index_of_skip(current_line, ' ', 1) + 1:)
            colour_code = current_line(1:1)
!            print *, cur, colour_code
            if (colour_code == "r") then
                max_r(i) = max(cur, max_r(i))
            elseif (colour_code == "g") then
                max_g(i) = max(cur, max_g(i))
            elseif (colour_code == "b") then
                max_b(i) = max(cur, max_b(i))
            end if

            if (occurs_first(current_line, ',', ';') == ',') then
                current_line = current_line(index_of(current_line, ',') + 1:)
            elseif (occurs_first(current_line, ';', ',') == ';') then
                current_line = current_line(index_of(current_line, ';') + 1:)
            else
                exit
            end if
        end do

!        part 1
        if ((max_r(i) <= 12) .and. (max_g(i) <= 13) .and. (max_b(i) <= 14)) then
            id_sum = id_sum + id
        end if

    end do

!   part 1
    print *, id_sum
!    part 2
    print *, sum(max_r * max_g * max_b)

end program day2