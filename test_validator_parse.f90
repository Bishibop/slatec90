program test_validator_parse
    implicit none
    real, allocatable :: real_array(:)
    character(len=200) :: line
    integer :: array_size, pos
    
    ! Test case that's failing
    line = "REAL_ARRAY: 0.0 0.0 0.0"
    pos = index(line, ':')
    
    ! First allocate the array
    array_size = 3
    allocate(real_array(array_size))
    
    ! Try to read
    print *, "Attempting to read from line: ", trim(line)
    print *, "Reading from position: ", pos+1
    print *, "Substring: '", line(pos+1:), "'"
    
    read(line(pos+1:), *) real_array
    
    print *, "Success! Read values: ", real_array
    
end program test_validator_parse