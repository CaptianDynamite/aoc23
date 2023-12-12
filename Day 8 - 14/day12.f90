module day12helper
    implicit none

    type view
        integer :: start, end
    end type view

contains

    impure function arrangement_count(str, runs, contig) result(count)
        character(len=*), intent(in) :: str
        type(view), intent(in) :: runs(:)
        integer, intent(in) :: contig(:)
        integer :: count
        if (size(contig) == 1) then

        end if
    end function

    pure function index_of(str, char) result(index)
        character(len=*), intent(in) :: str
        character, intent(in) :: char
        integer :: i
        integer :: index
        index = 0
        do i = 1, len(str)
            if (str(i:i) == char) then
                index = i
                exit
            end if
        end do
    end function index_of

    pure function str_occ(str, char) result(count)
        character(len=*), intent(in) :: str
        character, intent(in) :: char
        integer :: count
        integer :: i
        character :: prev
        count = 0
        prev = char
        do i = 1, len(str)
            if (prev /= char .and. str(i:i) == char) then
                count = count + 1
            end if
            prev = str(i:i)
        end do
    end function str_occ

    pure function str_split(str, char) result(indices)
        character(len=*), intent(in) :: str
        character, intent(in) :: char
        type(view), allocatable :: indices(:)
        integer :: occ, cur_index, i, substr
        character :: prev
        occ = str_occ(str, char)
        if (str(len(str):len(str)) == char) then
            allocate(indices(occ))
        else
            allocate(indices(occ + 1))
        end if
        cur_index = 1
        substr = 1
        prev = char
        do i = 1, len(str)
            if (prev /= str(i:i) .and. str(i:i) == char) then
                indices(substr) = view(cur_index, i - 1)
                substr = substr + 1
            end if
            if (str(i:i) == char) then
                cur_index = i + 1
            end if
            prev = str(i:i)
        end do
        if (str(len(str):len(str)) /= char) then
            indices(substr) = view(cur_index, len(str))
        end if
    end function str_split

end module day12helper

program day12
    use day12helper
    implicit none

    character(len=512) :: cur_line ! 512 is arbitrary but bigger than we need
    character(len=:), allocatable :: springs, run_length, contig_inf
    type(view), allocatable :: run_indices(:)
    integer, allocatable :: contig_runs(:)
    integer :: iostat, i, j, k, lines, split_index, contig_count
    integer :: part1

    open(22, file='day12.txt', status='old')
    lines = 0
    do while (.true.)
        read (22, '(A)', iostat=iostat) cur_line
        if (iostat /= 0) then
            exit
        end if
        lines = lines + 1
    end do
    rewind(22)
    part1 = 0
    do i = 1, lines
        read (22, '(A)', iostat=iostat) cur_line
        split_index = index_of(cur_line, ' ')
        springs = cur_line(1:split_index - 1)
        run_length = cur_line(split_index + 1:len(trim(cur_line)))
        run_indices = str_split(springs, '.')

        contig_inf = cur_line(split_index + 1 : len(trim(cur_line)))
        contig_count = str_occ(contig_inf, ',')
        allocate(contig_runs(contig_count + 1))
        read(contig_inf, *) contig_runs
        part1 = part1 + arrangement_count(springs, run_indices, contig_runs)
        deallocate(contig_runs)
    end do
    close(22)


end program day12