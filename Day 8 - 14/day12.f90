module day12helper
    use iso_fortran_env, only: int64
    implicit none

    type view
        integer :: start, end
    end type view

    type map_node
        integer(int64) :: entries(64, 16) ! arbitrary but bigger than we need
        integer :: entry_counts(16)
        integer(int64) :: vals(16)
    contains
        procedure :: get_if_present
        procedure :: save_val
    end type map_node

contains

    recursive subroutine arrangement_count(str, runs, contig, map, count)
        character(len=*), intent(in) :: str
        type(view), intent(in) :: runs(:)
        integer, intent(in) :: contig(:)
        type(map_node), intent(inout) :: map(:)
        integer(int64), intent(out) :: count
        type(view) :: cur_run
        type(view), allocatable :: mod_runs(:)
        integer(int64) :: rep(size(runs) + size(contig) + 1)
        integer :: i, j, pound_index, run_len
        integer(int64) :: hash, saved, temp

        rep = int_rep(str, runs, contig)
        hash = abs(mod(poly_roll_hash(rep), size(map)))

        saved = map(hash)%get_if_present(rep)
        if (saved /= -1) then
            count = saved
            return
        end if

        count = 0
        do i = 1, size(runs)
            cur_run = runs(i)
            run_len = cur_run%end - cur_run%start + 1
            pound_index = index_of(str(cur_run%start : cur_run%end), '#')
            if (pound_index == 0 .and. run_len < contig(1)) then
                cycle
            elseif (pound_index /= 0 .and. run_len < contig(1)) then
                exit
            end if

            if (pound_index > 0) then
                !TODO figure out how to account for multiple #
                do j = max(1, pound_index - contig(1) + 1), min( run_len - contig(1) + 1, pound_index + contig(1) - 1 )
                    if (cur_run%start + j + contig(1) > cur_run%end) then
                        mod_runs = runs(i + 1:)
                    else
                        mod_runs = runs(i:)
                        mod_runs(1) = view(cur_run%start + j + contig(1), cur_run%end)
                    end if

                    if (size(contig) == 1) then
                        count = count + 1
                    elseif (size(mod_runs) == 0) then
                        exit
                    else
                        call arrangement_count(str, mod_runs, contig(2:), map, temp)
                        count = count + temp
                    end if
                end do
                exit
            else
                do j = 1, run_len - contig(1) + 1
                    if (cur_run%start + j + contig(1) > cur_run%end) then
                        mod_runs = runs(i + 1:)
                    else
                        mod_runs = runs(i:)
                        mod_runs(1) = view(cur_run%start + j + contig(1), cur_run%end)
                    end if

                    if (size(contig) == 1) then
                        count = count + 1
                    elseif (size(mod_runs) == 0) then
                        exit
                    else
                        call arrangement_count(str, mod_runs, contig(2:), map, temp)
                        count = count + temp
                    end if
                end do
            end if
        end do
        call map(hash)%save_val(rep, count)
        print *, count, str, ' Runs: ',  size(runs), 'Contigs: ', contig
    end subroutine

    pure function get_if_present(self, rep) result(val)
        class(map_node), intent(in) :: self
        integer(int64), intent(in) :: rep(:)
        integer(int64) :: val
        integer :: i, cur_len
        val = -1
        do i = 1, size(self%entry_counts)
            cur_len = self%entry_counts(i)
            if (cur_len == 0) then
                exit
            end if
            if (cur_len == size(rep) .and. all(self%entries(1:cur_len, i) == rep)) then
                val = self%vals(i)
                exit
            end if
        end do
    end function get_if_present

    subroutine save_val(self, key, val)
        class(map_node), intent(inout) :: self
        integer(int64), intent(in) :: key(:)
        integer(int64), intent(in) :: val
        integer :: i
        do i = 1, size(self%entry_counts)
            if (self%entry_counts(i) == 0) then
                self%entry_counts(i) = size(key)
                self%entries(1:size(key), i) = key
                self%vals(i) = val
                return
            end if
        end do
        error stop 1 ! Any non-zero code works
    end subroutine save_val

    pure function int_rep(str, runs, contig) result(rep)
        character(len=*), intent(in) :: str
        type(view), intent(in) :: runs(:)
        integer, intent(in) :: contig(:)
        integer(int64) :: rep(size(runs) + size(contig) + 1)
        rep(:size(runs)) = cvt_view_to_num(str, runs)
        rep(size(runs) + 1) = 12345678910_int64 ! Sentinel value
        rep(size(runs) + 2:) = int8(contig)
    end function int_rep

    ! Note that this only produces hashes up to 65646776063023307 which is about 2^9 less than the maximum register size
    pure function poly_roll_hash(input) result(num)
        integer(int64), intent(in) :: input(:)
        integer(int64) :: num
        integer(int64), parameter :: p = 281
        integer(int64), parameter :: m = 36028797018963913_int64
        integer(int64) :: p_pow, temp, c
        integer :: i, j
        num = 0
        p_pow = p
        do i = 1, size(input)
            temp = input(i)
            do j = 1, 8
                c = mod(temp, 256)
                num = mod(num + p_pow * c, m)
                p_pow = mod(p_pow * p, m)
                temp = temp / 256
            end do
        end do
    end function poly_roll_hash

    pure elemental function cvt_view_to_num(str, v) result(num)
        character(len=*), intent(in) :: str
        type(view), intent(in) :: v
        integer(int64) :: num
        integer :: i
        character(len=:), allocatable :: str_view
        str_view = str(v%start : v%end)
        num = 0
        do i = 1, len(str_view)
            num = num * 3
            if (str_view(i:i) == '?') then
                num = num + 1
            elseif (str_view(i:i) == '#') then
                num = num + 2
            end if
        end do
    end function cvt_view_to_num

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
        count = 0
        do i = 1, len(str)
            if (char == str(i:i)) then
                count = count + 1
            end if
        end do
    end function str_occ

    pure function str_occ_coal(str, char) result(count)
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
    end function str_occ_coal

    pure function str_split(str, char) result(indices)
        character(len=*), intent(in) :: str
        character, intent(in) :: char
        type(view), allocatable :: indices(:)
        integer :: occ, cur_index, i, substr
        character :: prev
        occ = str_occ_coal(str, char)
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
    type(map_node), allocatable :: memoize(:)
    integer, allocatable :: contig_runs(:)
    integer :: iostat, i, j, k, lines, split_index, contig_count
    integer(int64) :: temp
    integer(int64) :: part1

    allocate(memoize(2 ** 16)) ! Arbitrary, but more than large enough
    memoize = map_node(0, 0, 0)

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
        contig_count = str_occ_coal(contig_inf, ',')
        allocate(contig_runs(contig_count + 1))
        read(contig_inf, *) contig_runs
        call arrangement_count(springs, run_indices, contig_runs, memoize, temp)
        print *, temp
        part1 = part1 + temp
        deallocate(contig_runs)
    end do

    print *, part1

    close(22)
    
end program day12