program test_bsplvn_full
    use bsplvn_module, only: bsplvn
    use iso_fortran_env, only: int32, real32
    implicit none
    
    integer :: i, j, test_num, iostat, t_size
    real :: t(100), vnikx(30), x
    integer :: jhigh, index_val, ileft, vnikx_size
    character(len=1000) :: line, desc
    character(len=50) :: dummy
    logical :: in_t_array
    
    open(10, file='test_data/bsplvn_tests_blind.json', status='old', action='read')
    
    test_num = 0
    in_t_array = .false.
    
    do
        read(10, '(A)', iostat=iostat) line
        if (iostat /= 0) exit
        
        ! Skip empty lines and brackets
        line = adjustl(line)
        if (len_trim(line) == 0) cycle
        if (line(1:1) == '[' .or. line(1:1) == ']') cycle
        if (line(1:1) == '{' .or. line(1:1) == '}') cycle
        
        ! Parse test case
        if (index(line, '"description"') > 0) then
            test_num = test_num + 1
            ! Extract description (not used, but we need to parse it)
            i = index(line, '": "') + 4
            j = index(line(i:), '"') + i - 2
            desc = line(i:j)
        else if (index(line, '"t"') > 0 .and. index(line, '[') > 0) then
            ! Start of t array
            in_t_array = .true.
            t_size = 0
            ! Check if array is on same line
            if (index(line, ']') > 0) then
                ! Parse inline array
                call parse_t_array_inline(line, t, t_size)
                in_t_array = .false.
            end if
        else if (in_t_array) then
            if (index(line, ']') > 0) then
                in_t_array = .false.
            else
                ! Parse t value
                read(line, *) t(t_size + 1)
                t_size = t_size + 1
            end if
        else if (index(line, '"jhigh"') > 0) then
            i = index(line, ': ') + 2
            read(line(i:), *) jhigh
        else if (index(line, '"index"') > 0) then
            i = index(line, ': ') + 2
            read(line(i:), *) index_val
        else if (index(line, '"x"') > 0) then
            i = index(line, ': ') + 2
            read(line(i:), *) x
        else if (index(line, '"ileft"') > 0) then
            i = index(line, ': ') + 2
            read(line(i:), *) ileft
        else if (index(line, '"vnikx_size"') > 0) then
            i = index(line, ': ') + 2
            j = index(line(i:), ',')
            if (j > 0) then
                read(line(i:i+j-2), *) vnikx_size
            else
                read(line(i:), *) vnikx_size
            end if
            
            ! We have all data for this test, run it
            do j = 1, 30
                vnikx(j) = 0.0
            end do
            
            call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
            
            ! Output results
            write(*, '(A,I5,A)', advance='no') 'TEST_', test_num, '_RESULT:'
            do j = 1, vnikx_size
                write(*, '(ES20.13)', advance='no') vnikx(j)
            end do
            write(*,*)
        end if
    end do
    
    close(10)
    
contains

    subroutine parse_t_array_inline(line, t, t_size)
        character(len=*), intent(in) :: line
        real, intent(out) :: t(*)
        integer, intent(out) :: t_size
        
        integer :: i, j, k
        character(len=20) :: num_str
        
        t_size = 0
        i = index(line, '[') + 1
        j = index(line, ']') - 1
        
        do while (i <= j)
            ! Skip spaces
            do while (i <= j .and. (line(i:i) == ' ' .or. line(i:i) == ','))
                i = i + 1
            end do
            if (i > j) exit
            
            ! Find end of number
            k = i
            do while (k <= j .and. line(k:k) /= ',' .and. line(k:k) /= ']')
                k = k + 1
            end do
            
            t_size = t_size + 1
            num_str = line(i:k-1)
            read(num_str, *) t(t_size)
            
            i = k + 1
        end do
    end subroutine parse_t_array_inline

end program test_bsplvn_full