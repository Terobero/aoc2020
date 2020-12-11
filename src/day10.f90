program day10
    use helper
    implicit none
    integer, allocatable :: list(:)
    integer(16), allocatable :: ways(:)
    integer :: diff(0:3)
    integer :: i

    list = read_int_list("inputs/day10.txt")
    call bubblesort(list)

    diff = 0
    diff(3) = 1 ! built-in adapter
    diff(list(1)) = diff(list(1)) + 1 ! outlet

    do i = 1, size(list)-1
        diff(list(i+1)-list(i)) = diff(list(i+1)-list(i)) + 1
    end do
    
    print '("Part 1 answer: ", I0)', diff(1)*diff(3)

    allocate(ways(-2:maxval(list)))
    ways = 0
    ways(0) = 1

    do i = 1, size(list)
        ways(list(i)) = ways(list(i)-1) + ways(list(i)-2) + ways(list(i)-3)
    end do

    print '("Part 2 answer: ", I0)', ways(size(ways)-3)
end program day10