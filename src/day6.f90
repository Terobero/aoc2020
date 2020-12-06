program day6
    use helper
    implicit none
    character(:), allocatable :: list(:)
    logical :: form(26), tmp(26)
    logical :: start
    integer :: i, j, total

    list = read_str_list("inputs/day6.txt")

    total = 0
    form = .false.
    do i = 1, size(list)
        do j = 1, len_trim(list(i))
            form(iachar(list(i)(j:j))-96) = .true.
        end do

        if (list(i) .eq. "" .or. i == size(list)) then
            do j = 1, 26
                if (form(j)) total = total + 1
            end do
            form = .false.
        end if
    end do

    print '("Part 1 answer: ", I0)', total

    total = 0
    form = .false.
    start = .true.
    do i = 1, size(list)
        tmp = .false.
        if (start) then
            do j = 1, len_trim(list(i))
                form(iachar(list(i)(j:j))-96) = .true.  
            end do
            start = .false.
        else if ((list(i) .eq. "") .eqv. .false.) then
            do j = 1, len_trim(list(i))
                tmp(iachar(list(i)(j:j))-96) = .true.
            end do
            form = form .and. tmp
        end if

        if (list(i) .eq. "" .or. i == size(list)) then
            do j = 1, 26
                if (form(j)) total = total + 1
            end do
            form = .false.
            start = .true.
        end if
    end do

    print '("Part 2 answer: ", I0)', total
end program day6