module day6helper
    implicit none
contains
    function string_whitespace(string) result(occ)
        character (len=*), intent(in) :: string
        integer :: occ
        integer :: i
        character :: prev
        occ = 0
        prev = ''
        do i = 1, len(string)
            if (string(i:i) == ' ' .and. string(i:i) /= prev) then
                occ = occ + 1
            end if
            prev = string(i:i)
        end do
    end function string_whitespace
end module day6helper

program day6
    use day6helper
    implicit none

    character(len=256) :: cur_line ! 256 is arbitrary but larger than we need
    character(len=:), allocatable :: scratch
    integer, allocatable :: times(:), distance_record(:), successful_times(:)
    integer, allocatable :: possible_times(:), distances(:)
    integer :: entries, io_res, i

    open(10, file="day6.txt", status="old")
    read(10, '(A)', iostat=io_res) cur_line
    scratch = trim(cur_line(6:))
    entries = string_whitespace(scratch) + 1
    allocate(times(entries))
    read(scratch, *) times

    read(10, '(A)', iostat=io_res) cur_line
    scratch = trim(cur_line(10:))
    allocate(distance_record(entries))
    read(scratch, *) distance_record

    close(10)

    allocate(successful_times(entries))

    do i = 1, size(times)
        possible_times = [( i, i=0, times(i) )]
        distances = (times(i) - possible_times) * possible_times
        successful_times(i) = count(distances > distance_record(i))
    end do
    print *, "Part 1: ", product(successful_times)


end program day6