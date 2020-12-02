module helper
    implicit none
contains
    function read_int_list(file_name) result(list)
        character(*), intent(in) :: file_name
        integer, allocatable :: list(:)
        integer :: i, stat, file_len

        open(unit=1, file=file_name, status="old", action="read")

        file_len = 0
        do
            read(1, *, iostat=stat)
            if (stat /= 0) exit
            file_len = file_len + 1
        end do
        
        allocate(list(file_len))
        rewind(1)
        
        do i = 1, file_len
            read(1, *) list(i)
        end do

        close(1)
    end function
end module helper