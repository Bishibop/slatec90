program test_xerhlt_modern
    use xerhlt_module
    implicit none
    
    ! Note: XERHLT calls STOP, so only one test case can be run per execution
    ! The test framework should handle this by running each test in a separate process
    
    ! Test with a sample error message
    call xerhlt("SLATEC     XERMSG     INVALID ERROR NUMBER")
    
    ! This line should never be reached
    print *, "ERROR: This line should not be executed"
    
end program test_xerhlt_modern