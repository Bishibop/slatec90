program generate_denorm_reference
    use denorm_module
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    
    integer, parameter :: TOTAL_TESTS = 257
    integer, parameter :: MAX_N = 10000
    
    real(real64) :: test_vectors(MAX_N)
    real(real64) :: result
    integer :: test_n
    character(len=200) :: test_desc
    
    integer :: json_unit, out_unit, ios
    integer :: current_test, vec_idx, i, j
    character(len=1000) :: line
    logical :: in_inputs
    
    ! Open files
    open(newunit=json_unit, file='test_data/denorm_tests_enhanced.json', &
         status='old', action='read', iostat=ios)
    if (ios /= 0) stop "Cannot open test data file"
    
    open(newunit=out_unit, file='validation/denorm_reference_complete.json', &
         status='replace', action='write')
    
    ! Write JSON header
    write(out_unit, '(A)') '{'
    write(out_unit, '(A)') '  "function": "denorm",'
    write(out_unit, '(A)') '  "implementation": "modern_fortran",'
    write(out_unit, '(A)') '  "results": ['
    
    current_test = 0
    in_inputs = .false.
    
    do
        read(json_unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        
        ! Parse test ID
        if (index(line, '"test_id"') > 0) then
            current_test = current_test + 1
            if (current_test > TOTAL_TESTS) exit
            
            ! Reset for new test
            vec_idx = 0
            in_inputs = .false.
            
        ! Parse description
        else if (index(line, '"description"') > 0 .and. current_test > 0) then
            i = index(line, '": "') + 4
            j = index(line(i:), '"') - 1
            test_desc = line(i:i+j-1)
            
        ! Parse n
        else if (index(line, '"n"') > 0 .and. index(line, '"inputs"') == 0 .and. current_test > 0) then
            i = index(line, ':')
            read(line(i+1:), *) test_n
            
        ! Parse inputs array start
        else if (index(line, '"inputs"') > 0 .and. current_test > 0) then
            in_inputs = .true.
            vec_idx = 0
            
        ! Parse input values
        else if (in_inputs .and. current_test > 0) then
            if (index(line, ']') > 0) then
                in_inputs = .false.
                
                ! Compute the result
                result = denorm(test_n, test_vectors(1:test_n))
                
                ! Write result
                write(out_unit, '(4X, E25.17)', advance='no') result
                if (current_test < TOTAL_TESTS) then
                    write(out_unit, '(A)') ','
                else
                    write(out_unit, '(A)') ''
                end if
                
                ! Progress indicator
                if (mod(current_test, 10) == 0) then
                    print '(A, I0, A)', "Processed ", current_test, " tests..."
                end if
                
            else
                ! Read a vector element
                line = adjustl(line)
                if (len_trim(line) > 0 .and. line(1:1) /= '[') then
                    vec_idx = vec_idx + 1
                    if (vec_idx <= MAX_N) then
                        ! Remove comma if present
                        if (line(len_trim(line):len_trim(line)) == ',') then
                            line = line(1:len_trim(line)-1)
                        end if
                        read(line, *) test_vectors(vec_idx)
                    end if
                end if
            end if
        end if
    end do
    
    ! Write JSON footer
    write(out_unit, '(A)') '  ],'
    write(out_unit, '(A, I0)') '  "total_results": ', current_test
    write(out_unit, '(A)') '}'
    
    close(json_unit)
    close(out_unit)
    
    print *, ""
    print '(A, I0, A)', "Generated ", current_test, " reference values"
    print *, "Output written to: validation/denorm_reference_complete.json"
    
end program generate_denorm_reference