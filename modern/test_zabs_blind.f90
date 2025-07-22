program test_zabs_blind
    use zabs_module
    use, intrinsic :: iso_fortran_env, only: real64, output_unit, error_unit
    implicit none
    
    ! Local variables
    integer :: i, ios, test_count
    double precision :: zr, zi, result
    character(len=512) :: line
    character(len=200) :: description
    logical :: in_test_cases, reading_inputs, reading_description
    integer :: input_count
    double precision :: inputs(2)
    
    ! Initialize
    test_count = 0
    in_test_cases = .false.
    reading_inputs = .false.
    reading_description = .false.
    input_count = 0
    
    ! Open the test data file
    open(unit=10, file='../test_data/zabs_tests_blind.json', &
         status='old', action='read', iostat=ios)
    if (ios /= 0) then
        write(error_unit, *) 'Error: Cannot open test data file'
        stop 1
    end if
    
    ! Write header for output
    write(output_unit, '(A)') '{'
    write(output_unit, '(A)') '  "function": "zabs",'
    write(output_unit, '(A)') '  "implementation": "modern",'
    write(output_unit, '(A)') '  "results": ['
    
    ! Read and process the file line by line
    do
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) exit
        
        ! Skip empty lines
        if (len_trim(line) == 0) cycle
        
        ! Check if we're in the test_cases section
        if (index(line, '"test_cases"') > 0) then
            in_test_cases = .true.
            cycle
        end if
        
        if (.not. in_test_cases) cycle
        
        ! Check for description
        if (index(line, '"description"') > 0) then
            reading_description = .true.
            ! Extract description
            i = index(line, ':', back=.true.)
            if (i > 0) then
                description = adjustl(line(i+1:))
                ! Remove quotes and comma
                i = index(description, '"')
                if (i > 0) description = description(i+1:)
                i = index(description, '"', back=.true.)
                if (i > 0) description = description(:i-1)
            end if
            cycle
        end if
        
        ! Check for inputs array start
        if (index(line, '"inputs"') > 0) then
            reading_inputs = .true.
            input_count = 0
            cycle
        end if
        
        ! Read input values
        if (reading_inputs) then
            if (index(line, ']') > 0) then
                ! End of inputs for this test case
                reading_inputs = .false.
                
                if (input_count == 2) then
                    ! Run the test
                    zr = inputs(1)
                    zi = inputs(2)
                    result = zabs(zr, zi)
                    
                    ! Output result
                    test_count = test_count + 1
                    if (test_count > 1) write(output_unit, '(A)', advance='no') ','
                    write(output_unit, '(A)') ''
                    write(output_unit, '(A)') '    {'
                    write(output_unit, '(A,A,A)') '      "description": "', trim(description), '",'
                    write(output_unit, '(A)') '      "inputs": ['
                    write(output_unit, '(A,E25.17,A)') '        ', zr, ','
                    write(output_unit, '(A,E25.17)') '        ', zi
                    write(output_unit, '(A)') '      ],'
                    write(output_unit, '(A,E25.17)') '      "result": ', result
                    write(output_unit, '(A)', advance='no') '    }'
                end if
                cycle
            end if
            
            ! Try to read a number from this line
            if (index(line, '[') == 0 .and. index(line, ']') == 0) then
                ! Remove comma if present
                i = index(line, ',')
                if (i > 0) line(i:i) = ' '
                
                read(line, *, iostat=ios) inputs(input_count + 1)
                if (ios == 0) then
                    input_count = input_count + 1
                end if
            end if
        end if
    end do
    
    ! Close JSON output
    write(output_unit, '(A)') ''
    write(output_unit, '(A)') '  ]'
    write(output_unit, '(A)') '}'
    
    ! Close file
    close(10)
    
    write(error_unit, '(A,I0,A)') 'Processed ', test_count, ' test cases'

end program test_zabs_blind