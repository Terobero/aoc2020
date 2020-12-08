program day8
    use helper
    implicit none
    character(:), allocatable :: list(:), list2(:)
    character(3) :: op
    character :: sign
    logical, allocatable :: visited(:)
    integer :: i, j, n, acc

    list = read_str_list("inputs/day8.txt")
    list2 = list

    allocate(visited(size(list)))

    acc = 0
    i = 1
    visited = .false.
    do
        if (i > size(list) .or. visited(i)) exit
        visited(i) = .true.

        op = list(i)(1:3)
        sign = list(i)(5:5)
        read(list(i)(6:), "(I10)") n

        if (op == "nop") then
            i = i + 1
            cycle
        else if (op == "acc") then
            if (sign == "+") then
                acc = acc + n
            else
                acc = acc - n
            endif
            i = i + 1
            cycle
        else if (op == "jmp") then
            if (sign == "+") then
                i = i + n
            else
                i = i - n
            endif
            cycle
        end if
    end do

    print '("Part 1 answer: ", I0)', acc

    j = 1
    outer: do
        list = list2
        if (list(j)(1:3) == "jmp") then
            list(j)(1:3) = "nop"
        else if (list(j)(1:3) == "nop") then
            list(j)(1:3) = "jmp"
        else
            j = j + 1
            cycle
        end if
        acc = 0
        i = 1
        visited = .false.
        do
            if (i > size(list)) exit outer
            if (visited(i)) then
                j = j + 1
                exit
            end if
            visited(i) = .true.

            op = list(i)(1:3)
            sign = list(i)(5:5)
            read(list(i)(6:), "(I10)") n

            if (op == "nop") then
                i = i + 1
                cycle
            else if (op == "acc") then
                if (sign == "+") then
                    acc = acc + n
                else
                    acc = acc - n
                endif
                i = i + 1
                cycle
            else if (op == "jmp") then
                if (sign == "+") then
                    i = i + n
                else
                    i = i - n
                endif
                cycle
            end if
        end do
    end do outer

    print '("Part 2 answer: ", I0)', acc
end program day8