```fortran
module ppsgf_module
    implicit none

contains

    function ppsgf(x, iz, c, a, bh) result(result)
        real :: ppsgf
        real, intent(in) :: x
        integer, intent(in) :: iz
        real, intent(in) :: c(:)
        real, intent(in) :: a(:)
        real, intent(in) :: bh(:)
        real :: sum
        integer :: j

        sum = 0.0
        do j = 1, iz
            sum = sum - 1.0/(x - bh(j))**2
        end do

        result = sum
    end function ppsgf

end module ppsgf_module

program test_ppsgf
    use ppsgf_module
    implicit none

    ! F77-compatible wrapper function
    real function ppsgf_f77(x, iz, c, a, bh)
        real :: x
        integer :: iz
        real :: c(*)
        real :: a(*)
        real :: bh(*)
        ppsgf_f77 = ppsgf(x, iz, c, a, bh)
    end function ppsgf_f77

    ! Add test code here if needed
end program test_ppsgf
```