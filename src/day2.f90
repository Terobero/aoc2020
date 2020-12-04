program day2
    implicit none
    character(100) :: line, tmp
    character :: c
    integer :: i, stat, valid, low, high, amount

    open(unit=1, file="inputs/day2.txt", status="old", action="read")
    
    valid = 0
    do
        read(1, "(A)", iostat=stat) line
        if (stat /= 0) exit

        tmp = line(1:index(line, "-")-1)
        read(tmp, "(I10)") low
        tmp = line(index(line, "-")+1:index(line, " ")-1)
        read(tmp, "(I10)") high

        line = line(index(line, " ")+1:)
        c = line(1:1)

        line = line(4:)
        amount = 0
        do i = 1, len(trim(line))
            if (line(i:i) == c) then
                amount = amount + 1
            end if
        end do
        
        if (low <= amount .and. amount <= high) then
            valid = valid + 1
        end if
    end do

    print '("Part 1 answer: ", I0)', valid

    rewind(1)

    valid = 0
    do
        read(1, "(A)", iostat=stat) line
        if (stat /= 0) exit

        tmp = line(1:index(line, "-")-1)
        read(tmp, "(I10)") low
        tmp = line(index(line, "-")+1:index(line, " ")-1)
        read(tmp, "(I10)") high

        line = line(index(line, " ")+1:)
        c = line(1:1)

        line = line(4:)
        amount = 0
        do i = 1, len(trim(line))
            if ((i == low .or. i == high) .and. line(i:i) == c) then
                amount = amount + 1
            end if
        end do
        
        if (amount == 1) then
            valid = valid + 1
        end if
    end do

    print '("Part 2 answer: ", I0)', valid
    
    close(1)
end program day2