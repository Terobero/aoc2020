program day4
    use helper
    use ftlregexmodule
    implicit none
    character(:), allocatable :: list(:)
    logical :: fields_present(7)
    character(3) :: fields(7)
    integer :: i, j, valid

    list = read_str_list("inputs/day4.txt")

    fields = (/ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" /)
    
    valid = 0
    fields_present = .false.
    do i = 1, size(list)
        do j = 1, 7
            if (index(list(i), fields(j)) /= 0) then
                fields_present(j) = .true.
            end if
        end do

        if (list(i) .eq. "" .or. i == size(list)) then
            if (all(fields_present)) then
                valid = valid + 1
            end if
            fields_present = .false.
        end if
    end do

    print '("Part 1 answer: ", I0)', valid

    valid = 0
    fields_present = .false.
    do i = 1, size(list)
        if(list(i) .matches. ftlRegex("byr:(19[2-9][0-9]|200[0-2])")) fields_present(1) = .true.

        if(list(i) .matches. ftlRegex("iyr:(201[0-9]|2020)")) fields_present(2) = .true.

        if(list(i) .matches. ftlRegex("eyr:(202[0-9]|2030)")) fields_present(3) = .true.

        if(list(i) .matches. ftlRegex("hgt:((1[5-8][0-9]|19[0-3])cm|(59|6[0-9]|7[0-6])in)")) fields_present(4) = .true.

        if(list(i) .matches. ftlRegex("hcl:#([0-9a-f]{6})\b")) fields_present(5) = .true.

        if(list(i) .matches. ftlRegex("ecl:(amb|blu|brn|gry|grn|hzl|oth)")) fields_present(6) = .true.

        if(list(i) .matches. ftlRegex("pid:([0-9]{9})\b")) fields_present(7) = .true.
        
        if (list(i) .eq. "" .or. i == size(list)) then
            if (all(fields_present)) then
                valid = valid + 1
            end if
            fields_present = .false.
        end if
    end do

    print '("Part 2 answer: ", I0)', valid
end program day4