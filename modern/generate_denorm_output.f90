program generate_denorm_output
    use denorm_module
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    
    ! Maximum array size for test vectors
    integer, parameter :: MAX_N = 1000
    
    ! Variables
    integer :: n, i, test_num, total_tests, ios
    double precision :: x(MAX_N)
    double precision :: result
    character(len=256) :: line
    character(len=1024) :: test_file, output_file
    logical :: reading_inputs, test_started
    
    ! Get test file path from command line or use default
    if (command_argument_count() > 0) then
        call get_command_argument(1, test_file)
    else
        test_file = '../test_data/denorm_tests_blind.json'
    end if
    
    if (command_argument_count() > 1) then
        call get_command_argument(2, output_file)
    else
        output_file = 'denorm_output.json'
    end if
    
    ! Open test file
    open(unit=10, file=trim(test_file), status='old', action='read', iostat=ios)
    if (ios /= 0) then
        write(error_unit, *) 'Error: Cannot open test file: ', trim(test_file)
        stop 1
    end if
    
    ! Open output file
    open(unit=20, file=trim(output_file), status='replace', action='write')
    
    ! Initialize
    test_num = 0
    reading_inputs = .false.
    test_started = .false.
    n = 0
    
    ! Write JSON header
    write(20, '(A)') '{'
    write(20, '(A)') '  "function": "denorm",'
    write(20, '(A)') '  "implementation": "modern_fortran",'
    write(20, '(A)') '  "results": ['
    
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
            ! For N=0 case, we might have empty inputs array on same line
            if (index(line, '[]') > 0) then
                ! Empty inputs - compute and output result immediately
                reading_inputs = .false.
                if (test_started) then
                    if (test_num > 0) write(20, '(A)') ','
                    test_num = test_num + 1
                    result = denorm(n, x)
                    
                    ! Output result in JSON format
                    write(20, '(A,G0.17,A)', advance='no') '    ', result, ''
                    
                    test_started = .false.
                end if
            end if
            cycle
        end if
        
        ! Read input values
        if (reading_inputs) then
            if (index(line, ']') > 0) then
                ! End of inputs - compute and output result
                reading_inputs = .false.
                if (test_started) then
                    if (test_num > 0) write(20, '(A)') ','
                    test_num = test_num + 1
                    result = denorm(n, x)
                    
                    ! Output result in JSON format
                    write(20, '(A,G0.17,A)', advance='no') '    ', result, ''
                    
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
    
    ! Write JSON footer
    write(20, *)
    write(20, '(A)') '  ],'
    write(20, '(A,I0)') '  "total_results": ', test_num
    write(20, '(A)') '}'
    
    close(20)
    
    write(*, '(A,I0,A)') 'Generated output for ', test_num, ' test cases'
    write(*, '(A,A)') 'Output written to: ', trim(output_file)
    
end program generate_denorm_output