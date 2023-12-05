module day5helper
    use iso_fortran_env, only: int64
    implicit none

    private
    public generate_mapping, apply_mappings, string_occurences

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

    elemental function map_points(this, point) result(mapping)
        class(triplet), intent(in) :: this
        integer(int64), intent(in) :: point
        integer(int64) :: mapping
        if ( this%src_from <= point .and. point <= (this%src_from + this%len - 1) ) then
            mapping = this%dest_from + point - this%src_from
        else
            mapping = -1
        end if
    end function

    function apply_mappings(range_maps, points) result(mapped)
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
    type(triplet), allocatable :: seed_soil(:), soil_fert(:), fert_watr(:), watr_ligt(:), ligt_temp(:), temp_humd(:), humd_locn(:)
    integer :: count, i, range_info(3)

    open(10, file='day5.txt', status='old')
    read (10, '(A)') line
    scratch_line = trim(line(8:)) ! To remove the seeds: prefix
    allocate( seed_nums(string_occurences( scratch_line, ' ' ) + 1) ) ! 1 Space between each number, so add 1
    read (scratch_line, *) seed_nums

    allocate(mapped(size(seed_nums)))


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

end program day5