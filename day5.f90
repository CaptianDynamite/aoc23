module day5helper
    use iso_fortran_env, only: int64
    implicit none

    private
    public generate_mapping, apply_mappings, string_occurences, map_ranges

    type, public :: range_double
        integer(int64) :: from, len
    end type

    type, public :: triplet
        integer(int64) :: dest_from, src_from, len
    contains
        procedure :: map_points
    end type

contains
    function generate_mapping(file_id) result(mapping)
        integer, intent(in) :: file_id
        type(triplet), allocatable :: mapping(:)
        character(len=256) :: line ! 256 is arbitrary but larger than any line length
        type(triplet) :: temp_range_info(128) ! 128 is arbitrary, but higher than we ever need.
        character(len=:), allocatable :: scratch_line
        integer :: count, eof
        count = 0
        do while (.true.)
            read(file_id, '(A)', iostat=eof) line
            if (eof /= 0) then
                exit
            end if
            scratch_line = trim(line)
            if (scratch_line(:) == '') then
                exit
            end if
            read(scratch_line, *) temp_range_info(count + 1)
            count = count + 1
        end do
        mapping = temp_range_info(1:count)
    end function

    pure function range_end(range) result(pos)
        type(range_double), intent(in) :: range
        integer(int64) :: pos
        pos = range%from + range%len - 1;
    end function

    pure function src_range(mapping) result(range)
        type(triplet), intent(in) :: mapping
        type(range_double) :: range
        range = range_double(mapping%src_from, mapping%len)
    end function

    pure function dest_range(mapping) result(range)
        type(triplet), intent(in) :: mapping
        type(range_double) :: range
        range = range_double(mapping%dest_from, mapping%len)
    end function

    pure function src_end(mapping) result(pos)
        type(triplet), intent(in) :: mapping
        integer(int64) :: pos
        pos = range_end(range_double( mapping%src_from, mapping%len ))
    end function

    pure function dest_end(mapping) result(pos)
        type(triplet), intent(in) :: mapping
        integer(int64) :: pos
        pos = range_end(range_double( mapping%dest_from, mapping%len ))
    end function

    pure function range_overlap(a, b) result(overlaps)
        type(range_double), intent(in) :: a, b
        logical :: overlaps
        overlaps = b%from < range_end(a) .and. a%from < range_end(b);
    end function

    function map_range(first, second) result(mappings)
        type(triplet), intent(in) :: first, second
        type(triplet):: mappings(3)
        type(triplet) :: lower_map, over_map, upper_map
        integer(int64) :: len_used, overlap_index

        ! We are using a triplet of -1 to signify that the range is empty
        lower_map = triplet(-1, -1, -1)
        over_map = triplet(-1, -1, -1)
        upper_map = triplet(-1, -1, -1)

        print *, first
        print *, second

        if (range_overlap(dest_range(first), src_range(second))) then
            if (second%src_from <= first%dest_from .and. dest_end(first) <= src_end(second)) then
                overlap_index = first%dest_from - second%src_from
                over_map = triplet( second%dest_from + overlap_index, first%src_from, first%len)
            elseif (first%dest_from < second%src_from .and. src_end(second) < dest_end(first)) then
                len_used = second%src_from - first%dest_from
                lower_map = triplet( first%dest_from, first%src_from, len_used)
                over_map = triplet(second%dest_from, first%src_from + len_used, second%len)
                len_used = len_used + second%len
                upper_map = triplet(dest_end(first) - len_used, first%src_from + len_used, dest_end(first) - src_end(second))
            elseif (first%dest_from < second%src_from .and. dest_end(first) <= src_end(second)) then
                print *, '!'
                len_used = second%src_from - first%dest_from
                lower_map = triplet( first%dest_from, first%src_from, len_used)
                over_map = triplet(second%dest_from, first%src_from + len_used, first%len - len_used)
            elseif (second%src_from <= first%dest_from .and. src_end(second) < dest_end(first)) then
                len_used = src_end(second) - first%dest_from + 1
                over_map = triplet(second%dest_from + (first%dest_from - second%src_from), first%src_from, len_used)
                upper_map = triplet(first%dest_from + len_used, first%src_from + len_used, first%len - len_used)
            end if
        end if
        mappings = [lower_map, over_map, upper_map]
    end function

    function map_ranges(first, second) result(mapping)
        type(triplet), intent(in) :: first(:), second(:)
        type(triplet), allocatable :: mapping(:)
        type(triplet) :: temp_map( size(first) * size(second) * 3 )
        type(triplet) :: potential_unmapped(size(second) * 2), cert_mapped(size(second))
        type(triplet) :: single_combination(3)
        integer :: i, j, k, temp_index, pot_index, cert_index
        logical :: transformed, discovered_double
        temp_index = 0

        do i = 1, size(first)
            pot_index = 0
            cert_index = 0
            transformed = .false.
            do j = 1, size(second)
                print *, "------------------------------------------------"
                single_combination = map_range(first(i), second(j))
                print *, single_combination(1)
                print *, single_combination(2)
                print *, single_combination(3)
                ! Always occurs with the following if statement being true
                if (single_combination(1)%src_from /= -1) then
                    pot_index = pot_index + 1
                    potential_unmapped(pot_index) = single_combination(1)
                end if
                if (single_combination(2)%src_from /= -1) then
                    temp_index = temp_index + 1
                    temp_map(temp_index) = single_combination(2)
                    cert_index = cert_index + 1
                    cert_mapped(cert_index) = single_combination(2)
                    transformed = .true.
                end if
                ! Always occurs with the above if statement being true
                if (single_combination(3)%src_from /= -1) then
                    pot_index = pot_index + 1
                    potential_unmapped(pot_index) = single_combination(3)
                end if
            end do
            if (pot_index > 0) then
                do j = 1, pot_index
                    discovered_double = .false.
                    do k = 1, cert_index
                        if ( range_overlap(src_range(potential_unmapped(j)), src_range(cert_mapped(k))) ) then
                            discovered_double = .true.
                            exit
                        end if
                    end do
                    if (.not. discovered_double) then
                        temp_index = temp_index + 1
                        temp_map(temp_index) = potential_unmapped(j)
                    end if
                end do
            end if
            if (.not. transformed) then
                temp_index = temp_index + 1
                temp_map(temp_index) = first(i)
            end if
        end do
        mapping = temp_map(1:temp_index)
    end function

    elemental function map_points(this, point) result(mapping)
        class(triplet), intent(in) :: this
        integer(int64), intent(in) :: point
        integer(int64) :: mapping
        if ( this%src_from <= point .and. point <= src_end(this) ) then
            mapping = this%dest_from + point - this%src_from
        else
            mapping = -1
        end if
    end function

    pure function apply_mappings(range_maps, points) result(mapped)
        type(triplet), intent(in) :: range_maps(:)
        integer(int64), intent(in) :: points(:)
        integer(int64) :: mapped(size(points))
        integer :: i
        mapped = -1
        do i = 1, size(range_maps)
            mapped = max(mapped, range_maps(i)%map_points(points))
        end do
        where (mapped == -1)
            mapped = points
        end where
    end function

    ! Calculate how many times ``char`` appears in ``string``
    function string_occurences(string, char) result(count)
        character(len=*), intent(in) :: string
        character, intent(in) :: char
        integer :: count
        integer :: i
        count = 0
        do i = 1, len(string)
            if (string(i:i) == char) then
                count = count + 1
            end if
        end do
    end function string_occurences

end module day5helper

program day5
    use iso_fortran_env, only: int64
    use day5helper
    implicit none
    character(len=256) :: line ! 256 is arbitrary but larger than any line length
    character(len=:), allocatable :: scratch_line
    integer(int64), allocatable :: seed_nums(:), mapped(:)
    integer(int64), allocatable :: range(:)
    type(triplet), allocatable :: seed_mappings(:)
    type(triplet), allocatable :: seed_soil(:), soil_fert(:), fert_watr(:), watr_ligt(:), ligt_temp(:), temp_humd(:), humd_locn(:)
    type(triplet), allocatable :: combined(:)
    integer(int64) :: count, i, j, temp1, temp2, range_info(3)

    open(10, file='day5.txt', status='old')
    read (10, '(A)') line
    scratch_line = trim(line(8:)) ! To remove the seeds: prefix
    allocate( seed_nums(string_occurences( scratch_line, ' ' ) + 1) ) ! 1 Space between each number, so add 1
    read (scratch_line, *) seed_nums

    allocate(mapped(size(seed_nums)))
    allocate(seed_mappings(size(seed_nums) / 2))
    do i = 1, size(seed_nums), 2
        seed_mappings(i / 2 + 1) = triplet(seed_nums(i), seed_nums(i), seed_nums(i + 1))
    end do

    read(10, '(A)') line ! Blank line in input
    read(10, '(A)') line ! "seed-to-soil map:"
    seed_soil = generate_mapping(10)
    read(10, '(A)') line ! "soil-to-fertilizer map:"
    soil_fert = generate_mapping(10)
    read(10, '(A)') line ! "fertilizer-to-water map:"
    fert_watr = generate_mapping(10)
    read(10, '(A)') line ! "water-to-light map:"
    watr_ligt = generate_mapping(10)
    read(10, '(A)') line ! "light-to-temperature map:"
    ligt_temp = generate_mapping(10)
    read(10, '(A)') line ! "temperature-to-humidity map:"
    temp_humd = generate_mapping(10)
    read(10, '(A)') line ! "humidity-to-location map:"
    humd_locn = generate_mapping(10)

    mapped = apply_mappings(seed_soil, seed_nums)
    mapped = apply_mappings(soil_fert, mapped)
    mapped = apply_mappings(fert_watr, mapped)
    mapped = apply_mappings(watr_ligt, mapped)
    mapped = apply_mappings(ligt_temp, mapped)
    mapped = apply_mappings(temp_humd, mapped)
    mapped = apply_mappings(humd_locn, mapped)

    print *, "Part 1: ", minval(mapped)

    combined = map_ranges(seed_mappings, seed_soil)
    print *, 'Seed Soil: '
    print '(I3 I3 I3)', combined
    combined = map_ranges(combined, soil_fert)
    print *, 'Soil Fertiliser: '
    print '(I3 I3 I3)', combined
    combined = map_ranges(combined, fert_watr)
    print *, 'Fertiliser Water: '
    print '(I3 I3 I3)', combined
    combined = map_ranges(combined, watr_ligt)
    print *, 'Water Light: '
    print '(I3 I3 I3)', combined
    combined = map_ranges(combined, ligt_temp)
    print *, 'Light Temp: '
    print '(I3 I3 I3)', combined
    combined = map_ranges(combined, temp_humd)
    print *, 'Temp Humidity: '
    print '(I3 I3 I3)', combined
    combined = map_ranges(combined, humd_locn)
    print *, 'Humidity Location: '
    print '(I3 I3 I3)', combined

    print *, "Part 2: ", apply_mappings(combined, [82_int64])

end program day5