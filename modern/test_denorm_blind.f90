program test_denorm_blind
    use denorm_module
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    
    ! Maximum array size for test vectors
    integer, parameter :: MAX_N = 1000
    
    ! Variables
    integer :: n, i, test_num, total_tests, ios
    double precision :: x(MAX_N)
    double precision :: result
    character(len=256) :: line, description
    character(len=1024) :: test_file
    logical :: reading_inputs, test_started
    
    ! Get test file path from command line or use default
    if (command_argument_count() > 0) then
        call get_command_argument(1, test_file)
    else
        test_file = '../test_data/denorm_tests_blind.json'
    end if
    
    ! Open test file
    open(unit=10, file=trim(test_file), status='old', action='read', iostat=ios)
    if (ios /= 0) then
        write(error_unit, *) 'Error: Cannot open test file: ', trim(test_file)
        stop 1
    end if
    
    ! Initialize
    test_num = 0
    reading_inputs = .false.
    test_started = .false.
    n = 0
    
    ! Print header
    write(*, '(A)') 'DENORM Modern Fortran - Blind Test Results'
    write(*, '(A)') '=========================================='
    write(*, *)
    
    ! Read and process test file line by line
    do
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) exit
        
        ! Skip empty lines and opening braces
        line = adjustl(line)
        if (len_trim(line) == 0 .or. line(1:1) == '{') cycle
        
        ! Extract total tests count
        if (index(line, '"total_tests"') > 0) then
            i = index(line, ':')
            if (i > 0) then
                read(line(i+1:), *) total_tests
            end if
        end if
        
        ! Extract test description
        if (index(line, '"description"') > 0) then
            i = index(line, ':', back=.false.)
            if (i > 0) then
                description = line(i+1:)
                ! Remove quotes and trailing comma
                i = index(description, '"')
                if (i > 0) description = description(i+1:)
                i = index(description, '"', back=.true.)
                if (i > 0) description = description(1:i-1)
            end if
        end if
        
        ! Extract n value
        if (index(line, '"n"') > 0 .and. index(line, '"inputs"') == 0) then
            i = index(line, ':')
            if (i > 0) then
                read(line(i+1:), *) n
                test_started = .true.
            end if
        end if
        
        ! Check for inputs array start
        if (index(line, '"inputs"') > 0) then
            reading_inputs = .true.
            x = 0.0D0
            i = 0
            cycle
        end if
        
        ! Read input values
        if (reading_inputs) then
            if (index(line, ']') > 0) then
                ! End of inputs - compute and output result
                reading_inputs = .false.
                if (test_started) then
                    test_num = test_num + 1
                    result = denorm(n, x)
                    
                    ! Output in structured format
                    write(*, '(A,I3,A)') 'Test ', test_num, ':'
                    write(*, '(A,A)') '  Description: ', trim(description)
                    write(*, '(A,I0)') '  N: ', n
                    write(*, '(A)', advance='no') '  Inputs: ['
                    if (n > 0) then
                        do i = 1, min(n, 5)
                            if (i > 1) write(*, '(A)', advance='no') ', '
                            write(*, '(G0.15)', advance='no') x(i)
                        end do
                        if (n > 5) write(*, '(A)', advance='no') ', ...'
                    end if
                    write(*, '(A)') ']'
                    write(*, '(A,G0.15)') '  Result: ', result
                    write(*, *)
                    
                    test_started = .false.
                end if
            else if (index(line, '[') == 0 .and. len_trim(line) > 0) then
                ! Read a numeric value
                i = i + 1
                if (i <= MAX_N .and. i <= n) then
                    read(line, *) x(i)
                end if
            end if
        end if
    end do
    
    close(10)
    
    write(*, '(A,I0,A)') 'Total tests processed: ', test_num, ' tests'
    
end program test_denorm_blind