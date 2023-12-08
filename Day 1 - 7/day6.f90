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
    use iso_fortran_env, only: int64
    implicit none

    character(len=256) :: cur_line ! 256 is arbitrary but larger than we need
    character(len=:), allocatable :: scratch
    integer(int64), allocatable :: times(:), distance_record(:), successful_times(:)
    integer(int64), allocatable :: possible_times(:), distances(:)
    integer(int64) :: kerned_time, kerned_distance
    integer :: entries, io_res, i, j
    integer(int64) :: beginning, end, rate

    call system_clock(beginning, rate)

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

    call system_clock(end)
    print *, "File IO took ", end - beginning, " clock ticks, with ", rate, " clock ticks/s"
    call system_clock(beginning)

    allocate(successful_times(entries))

    do i = 1, size(times)
        possible_times = [( j, j=0, times(i) )]
        distances = (times(i) - possible_times) * possible_times
        successful_times(i) = count(distances > distance_record(i))
    end do
    print *, "Part 1: ", product(successful_times)

    call system_clock(end)
    print *, "Part 1 took ", end - beginning, " clock ticks, with ", rate, " clock ticks/s"
    call system_clock(beginning)

    kerned_time = 0
    kerned_distance = 0
    do i = 1, size(times)
        kerned_time = kerned_time * (10 ** ceiling(log10(real(times(i)))))
        kerned_time = kerned_time + times(i)
        kerned_distance = kerned_distance * (10 ** ceiling(log10(real(distance_record(i)))))
        kerned_distance = kerned_distance + distance_record(i)
    end do

    possible_times = [( i, i=0, kerned_time )]
    distances = (kerned_time - possible_times) * possible_times
    print *, "Part 2: ", int8(count(distances > kerned_distance))

    call system_clock(end)
    print *, "Part 2 took ", end - beginning, " clock ticks, with ", rate, " clock ticks/s"

end program day6