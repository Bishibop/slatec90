program test_debug_large
    implicit none
    
    ! Simulate first iteration of BSPLVN with large knots
    real :: t(5) = [1.0e10, 2.0e10, 3.0e10, 4.0e10, 5.0e10]
    real :: x = 2.5e10
    integer :: ileft = 3
    real :: vnikx(4)
    
    ! Variables for j=1 iteration
    real :: deltap(1), deltam(1)
    real :: vm, vmprev
    integer :: ipj, imjp1
    
    print *, "Simulating BSPLVN j=1 iteration with large knots"
    print *, "t =", t
    print *, "x =", x
    print *, "ileft =", ileft
    
    ! Initialize
    vnikx = 0.0
    vnikx(1) = 1.0
    
    ! j=1 computation
    ipj = ileft + 1  ! = 3 + 1 = 4
    imjp1 = ileft - 1 + 1  ! = 3 - 1 + 1 = 3
    
    deltap(1) = t(ipj) - x     ! t(4) - x = 4e10 - 2.5e10 = 1.5e10
    deltam(1) = x - t(imjp1)   ! x - t(3) = 2.5e10 - 3e10 = -0.5e10
    
    print *, ""
    print *, "j=1 calculations:"
    print *, "ipj =", ipj, "t(ipj) =", t(ipj)
    print *, "imjp1 =", imjp1, "t(imjp1) =", t(imjp1)
    print *, "deltap(1) = t(4) - x =", t(ipj), "-", x, "=", deltap(1)
    print *, "deltam(1) = x - t(3) =", x, "-", t(imjp1), "=", deltam(1)
    
    print *, ""
    print *, "ERROR: deltam(1) is NEGATIVE!"
    print *, "This happens because x < t(imjp1), which means we're"
    print *, "outside the support of the B-spline basis function!"
    
    ! Cox-de Boor recursion for l=1
    vmprev = 0.0
    vm = vnikx(1) / (deltap(1) + deltam(1))
    print *, ""
    print *, "Cox-de Boor recursion:"
    print *, "vm = vnikx(1) / (deltap(1) + deltam(1))"
    print *, "   = 1.0 / (", deltap(1), "+", deltam(1), ")"
    print *, "   = 1.0 / ", deltap(1) + deltam(1)
    print *, "   =", vm
    
    vnikx(1) = vm * deltap(1) + vmprev
    vmprev = vm * deltam(1)
    
    print *, ""
    print *, "vnikx(1) = vm * deltap(1) =", vm, "*", deltap(1), "=", vnikx(1)
    print *, "vnikx(2) = vm * deltam(1) =", vm, "*", deltam(1), "=", vmprev
    
    print *, ""
    print *, "Since deltam(1) is negative, vnikx(2) becomes negative!"
    
end program test_debug_large