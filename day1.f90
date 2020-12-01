program day1
    use helper
    implicit none
    integer, allocatable :: list(:)
    integer :: i, j, k

    list = read_int_list("inputs/day1.txt")

    outer: do i = 1, size(list)-1
        do j = i+1, size(list)
            if (list(i) + list(j) == 2020) then
                print '("Part 1 answer: ", I0)', list(i) * list(j)
                exit outer
            end if
        end do
    end do outer

    outer2: do i = 1, size(list)-2
        do j = i+1, size(list)-1
            do k = i+2, size(list)
                if (list(i) + list(j) + list(k) == 2020) then
                    print '("Part 2 answer: ", I0)', list(i) * list(j) * list(k)
                    exit outer2
                end if
            end do
        end do
    end do outer2
end program day1