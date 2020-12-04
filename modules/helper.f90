module helper
    implicit none
contains
    function longest_line_length(unit) result(max_line_length)
        character(10000) :: line
        integer, intent(in) :: unit
        integer :: max_line_length, stat

        max_line_length = 0
        do
            read(unit, "(a)", iostat=stat) line
            if (stat /= 0) exit

            if (len_trim(line) > max_line_length) then
                max_line_length = len_trim(line)
            end if
        end do

        rewind(unit)
    end function longest_line_length

    function count_file_lines(unit) result(line_count)
        integer, intent(in) :: unit
        integer :: line_count, stat

        line_count = 0
        do
            read(unit, "(a)", iostat=stat)
            if (stat /= 0) exit
            line_count = line_count + 1
        end do
        
        rewind(unit)
    end function count_file_lines

    function read_int_list(file_name) result(list)
        character(*), intent(in) :: file_name
        integer, allocatable :: list(:)
        integer :: i, file_len

        open(unit=15, file=file_name, status="old", action="read")

        file_len = count_file_lines(15)
        
        allocate(list(file_len))
        
        do i = 1, file_len
            read(15, *) list(i)
        end do

        close(15)
    end function

    function read_str_list(file_name) result(list)
        character(*), intent(in) :: file_name
        character(:), allocatable :: list(:)
        integer :: i, file_len

        open(unit=15, file=file_name, status="old", action="read")

        file_len = count_file_lines(15)
        
        allocate(character(longest_line_length(15)) :: list(file_len))
        
        do i = 1, file_len
            read(15, "(a)") list(i)
        end do

        close(15)
    end function
end module helper