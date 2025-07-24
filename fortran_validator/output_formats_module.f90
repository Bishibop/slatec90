module output_formats_module
    use error_analysis_module
    implicit none
    
    ! Output format constants
    integer, parameter :: FORMAT_HUMAN = 1
    integer, parameter :: FORMAT_JSON = 2
    integer, parameter :: FORMAT_LLM = 3
    integer, parameter :: FORMAT_JUNIT = 4
    
    ! Current output format
    integer :: output_format = FORMAT_HUMAN
    
    ! JSON state tracking
    logical :: json_first_test = .true.
    logical :: json_first_function = .true.
    
contains

    subroutine set_output_format(format_string)
        character(len=*), intent(in) :: format_string
        
        select case(trim(adjustl(format_string)))
            case('human')
                output_format = FORMAT_HUMAN
            case('json')
                output_format = FORMAT_JSON
            case('llm')
                output_format = FORMAT_LLM
            case('junit', 'xml')
                output_format = FORMAT_JUNIT
            case default
                print *, 'Warning: Unknown format "', trim(format_string), '", using human format'
                output_format = FORMAT_HUMAN
        end select
    end subroutine set_output_format
    
    subroutine output_header()
        select case(output_format)
            case(FORMAT_JSON)
                print '(A)', '{"validation_results": {'
                print '(A)', '  "timestamp": "' // get_timestamp() // '",'
                print '(A)', '  "functions": ['
                json_first_function = .true.
                
            case(FORMAT_JUNIT)
                print '(A)', '<?xml version="1.0" encoding="UTF-8"?>'
                print '(A)', '<testsuites name="SLATEC Validation">'
                
            case(FORMAT_LLM)
                print '(A)', '# SLATEC Validation Report'
                print '(A)', '## Timestamp: ' // get_timestamp()
                print '(A)', ''
        end select
    end subroutine output_header
    
    subroutine output_function_start(function_name)
        character(len=*), intent(in) :: function_name
        
        select case(output_format)
            case(FORMAT_HUMAN)
                print '(A)', repeat('=', 60)
                print '(A,A)', 'VALIDATING FUNCTION: ', trim(function_name)
                print '(A)', repeat('=', 60)
                
            case(FORMAT_JSON)
                if (.not. json_first_function) print '(A)', ','
                print '(A)', '    {'
                print '(A)', '      "function": "' // trim(function_name) // '",'
                print '(A)', '      "tests": ['
                json_first_test = .true.
                json_first_function = .false.
                
            case(FORMAT_JUNIT)
                print '(A)', '  <testsuite name="' // trim(function_name) // '">'
                
            case(FORMAT_LLM)
                print '(A)', '## Function: ' // trim(function_name)
                print '(A)', ''
        end select
    end subroutine output_function_start
    
    subroutine output_test_result(test_desc, passed, f77_val, modern_val, error_info)
        character(len=*), intent(in) :: test_desc
        logical, intent(in) :: passed
        real, intent(in), optional :: f77_val, modern_val
        character(len=*), intent(in), optional :: error_info
        
        integer :: error_type
        character(len=200) :: error_msg, suggestion
        
        select case(output_format)
            case(FORMAT_HUMAN)
                if (passed) then
                    print '(A,A)', 'PASS: ', trim(test_desc)
                else
                    print '(A,A)', 'FAIL: ', trim(test_desc)
                    if (present(f77_val) .and. present(modern_val)) then
                        print '(A,ES16.8)', '  F77 result:    ', f77_val
                        print '(A,ES16.8)', '  Modern result: ', modern_val
                        print '(A,ES16.8)', '  Difference:    ', abs(f77_val - modern_val)
                        if (abs(f77_val) > 0.0) then
                            print '(A,F6.2,A)', '  Relative err:  ', &
                                100.0 * abs(f77_val - modern_val) / abs(f77_val), '%'
                        end if
                    end if
                    if (present(error_info)) then
                        print '(A,A)', '  Details: ', trim(error_info)
                    end if
                end if
                
            case(FORMAT_JSON)
                if (.not. json_first_test) print '(A)', ','
                print '(A)', '        {'
                print '(A)', '          "description": "' // escape_json(test_desc) // '",'
                if (passed) then
                    print '(A)', '          "passed": true,'
                else
                    print '(A)', '          "passed": false,'
                end if
                if (present(f77_val) .and. present(modern_val)) then
                    print '(A,ES16.8,A)', '          "f77_result": ', f77_val, ','
                    print '(A,ES16.8,A)', '          "modern_result": ', modern_val, ','
                    print '(A,ES16.8)', '          "difference": ', abs(f77_val - modern_val)
                else
                    print '(A)', '          "details": "' // escape_json(error_info) // '"'
                end if
                print '(A)', '        }'
                json_first_test = .false.
                
            case(FORMAT_JUNIT)
                print '(A)', '    <testcase name="' // escape_xml(test_desc) // '">'
                if (.not. passed) then
                    print '(A)', '      <failure>'
                    if (present(f77_val) .and. present(modern_val)) then
                        write(*, '(A,ES16.8)') '        F77 result: ', f77_val
                        write(*, '(A,ES16.8)') '        Modern result: ', modern_val
                    end if
                    if (present(error_info)) then
                        print '(A)', '        ' // escape_xml(error_info)
                    end if
                    print '(A)', '      </failure>'
                end if
                print '(A)', '    </testcase>'
                
            case(FORMAT_LLM)
                if (.not. passed .and. present(f77_val) .and. present(modern_val)) then
                    ! Detailed analysis for LLM consumption
                    call analyze_error(f77_val, modern_val, test_desc, error_type, error_msg)
                    call suggest_fix(error_type, test_desc, suggestion)
                    
                    print '(A)', '### Failed Test: ' // trim(test_desc)
                    print '(A)', '```'
                    print '(A)', 'Error Category: ' // get_error_category(error_type)
                    print '(A,ES16.8)', 'F77 Result:     ', f77_val
                    print '(A,ES16.8)', 'Modern Result:  ', modern_val
                    print '(A,ES16.8)', 'Difference:     ', abs(f77_val - modern_val)
                    if (abs(f77_val) > 0.0) then
                        print '(A,F8.4,A)', 'Relative Error: ', &
                            100.0 * abs(f77_val - modern_val) / abs(f77_val), '%'
                    end if
                    print '(A)', ''
                    print '(A)', 'Analysis: ' // trim(error_msg)
                    print '(A)', 'Suggestion: ' // trim(suggestion)
                    print '(A)', '```'
                    print '(A)', ''
                end if
        end select
    end subroutine output_test_result
    
    subroutine output_function_end(test_count, passed_count, failed_count)
        integer, intent(in) :: test_count, passed_count, failed_count
        
        select case(output_format)
            case(FORMAT_JSON)
                print '(A)', '      ],'
                print '(A,I0,A)', '      "total_tests": ', test_count, ','
                print '(A,I0,A)', '      "passed": ', passed_count, ','
                print '(A,I0)', '      "failed": ', failed_count
                print '(A)', '    }'
                
            case(FORMAT_JUNIT)
                print '(A,I0,A,I0,A,I0,A)', '  </testsuite> <!-- tests="', &
                    test_count, '" failures="', failed_count, '" -->'
                    
            case(FORMAT_LLM)
                if (failed_count > 0) then
                    print '(A,I0,A,I0,A)', '**Summary**: ', failed_count, ' of ', &
                        test_count, ' tests failed'
                else
                    print '(A,I0,A)', '**Summary**: All ', test_count, ' tests passed ✓'
                end if
                print '(A)', ''
        end select
    end subroutine output_function_end
    
    subroutine output_footer(total_tests, total_passed, total_failed)
        integer, intent(in) :: total_tests, total_passed, total_failed
        real :: pass_rate
        
        if (total_tests > 0) then
            pass_rate = 100.0 * real(total_passed) / real(total_tests)
        else
            pass_rate = 0.0
        end if
        
        select case(output_format)
            case(FORMAT_HUMAN)
                print '(A)', ''
                print '(A)', repeat('=', 60)
                print '(A)', 'VALIDATION SUMMARY'
                print '(A)', repeat('=', 60)
                print '(A,I5)', 'Total tests:  ', total_tests
                print '(A,I5)', 'Passed:       ', total_passed
                print '(A,I5)', 'Failed:       ', total_failed
                if (total_tests > 0) then
                    print '(A,F6.2,A)', 'Pass rate:    ', pass_rate, '%'
                end if
                
            case(FORMAT_JSON)
                print '(A)', '  ],'
                print '(A,I0)', '  "summary": {'
                print '(A,I0,A)', '    "total_tests": ', total_tests, ','
                print '(A,I0,A)', '    "total_passed": ', total_passed, ','
                print '(A,I0,A)', '    "total_failed": ', total_failed, ','
                print '(A,F6.2)', '    "pass_rate": ', pass_rate
                print '(A)', '  }'
                print '(A)', '}}'
                
            case(FORMAT_JUNIT)
                print '(A,I0,A,I0,A)', '</testsuites> <!-- tests="', total_tests, &
                    '" failures="', total_failed, '" -->'
                    
            case(FORMAT_LLM)
                print '(A)', '## Overall Summary'
                print '(A)', ''
                if (total_failed == 0) then
                    print '(A)', '✅ **All tests passed!**'
                else
                    print '(A)', '❌ **Validation failed**'
                end if
                print '(A)', ''
                print '(A,I0)', '- Total tests: ', total_tests
                print '(A,I0)', '- Passed: ', total_passed
                print '(A,I0)', '- Failed: ', total_failed
                print '(A,F6.2,A)', '- Pass rate: ', pass_rate, '%'
        end select
    end subroutine output_footer
    
    ! Helper functions
    function get_timestamp() result(timestamp)
        character(len=19) :: timestamp
        integer :: values(8)
        
        call date_and_time(values=values)
        write(timestamp, '(I4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)') &
            values(1), '-', values(2), '-', values(3), ' ', &
            values(5), ':', values(6), ':', values(7)
    end function get_timestamp
    
    function escape_json(str) result(escaped)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: escaped
        integer :: i, n
        
        n = len_trim(str)
        escaped = ''
        
        do i = 1, n
            select case(str(i:i))
                case('"')
                    escaped = escaped // '\"'
                case('\')
                    escaped = escaped // '\\'
                case(char(10))
                    escaped = escaped // '\n'
                case(char(13))
                    escaped = escaped // '\r'
                case default
                    escaped = escaped // str(i:i)
            end select
        end do
    end function escape_json
    
    function escape_xml(str) result(escaped)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: escaped
        integer :: i, n
        
        n = len_trim(str)
        escaped = ''
        
        do i = 1, n
            select case(str(i:i))
                case('<')
                    escaped = escaped // '&lt;'
                case('>')
                    escaped = escaped // '&gt;'
                case('&')
                    escaped = escaped // '&amp;'
                case('"')
                    escaped = escaped // '&quot;'
                case("'")
                    escaped = escaped // '&apos;'
                case default
                    escaped = escaped // str(i:i)
            end select
        end do
    end function escape_xml
    
    function get_error_category(error_type) result(category)
        integer, intent(in) :: error_type
        character(len=30) :: category
        
        select case(error_type)
            case(ERROR_NAN_HANDLING)
                category = 'NaN Handling Difference'
            case(ERROR_ZERO_NONZERO)
                category = 'Zero vs Non-Zero'
            case(ERROR_SIGN_DIFF)
                category = 'Sign Difference'
            case(ERROR_LARGE_REL)
                category = 'Large Relative Error'
            case(ERROR_INFINITY)
                category = 'Infinity Handling'
            case(ERROR_UNDERFLOW)
                category = 'Underflow'
            case default
                category = 'Numerical Difference'
        end select
    end function get_error_category

end module output_formats_module