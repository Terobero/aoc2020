program day3
    use helper
    implicit none
    character(:), allocatable :: list(:)
    integer(16) :: trees(5)
    integer, dimension(5) :: x_positions, x_change, y_change
    integer :: i, line_no

    list = read_str_list("inputs/day3.txt")

    x_change = (/ 1, 3, 5, 7, 1 /)
    y_change = (/ 1, 1, 1, 1, 2 /)
    trees = 0
    x_positions = 1

    do line_no = 2, size(list) ! Skip first line
        do i = 1, 5
            x_positions(i) = x_positions(i) + x_change(i)
            if (x_positions(i) > 31) then
                x_positions(i) = mod(x_positions(i), 31)
            end if

            if (mod(line_no-1, y_change(i)) == 0 .and. list(line_no)(x_positions(i):x_positions(i)) == "#") then
                trees(i) = trees(i) + 1
            end if
        end do
    end do

    print '("Part 1 answer: ", I0)', trees(2)
    print '("Part 2 answer: ", I0)', product(trees)
end program day3