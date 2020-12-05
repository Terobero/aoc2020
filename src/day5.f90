program day5
    use helper
    implicit none
    character(:), allocatable :: list(:)
    integer, allocatable :: seats(:)
    integer :: i

    list = read_str_list("inputs/day5.txt")
    allocate(seats(size(list)))
    seats = 0

    do i = 1, 7
        where (list(:)(i:i) == "B") seats = seats + 2**(7-i) * 8
    end do

    do i = 8, 10
        where (list(:)(i:i) == "R") seats = seats + 2**(4-i+6)
    end do

    print '("Part 1 answer: ", I0)', maxval(seats)

    do i = 0, 1023
        if (any(seats == i-1) .and. any(seats == i+1) .and. all(seats /= i)) then
            print '("Part 2 answer: ", I0)', i
        end if
    end do
end program day5