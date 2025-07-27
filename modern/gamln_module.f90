module gamln_module
  use iso_fortran_env, only: real32
  use r1mach_module, only: r1mach
  use i1mach_module, only: i1mach
  implicit none
  private
  public :: gamln

contains

  function gamln(z, ierr) result(res)
    implicit none
    ! Input and output declarations
    real(real32), intent(in)  :: z
    integer,       intent(out) :: ierr
    real(real32)              :: res

    ! Local variables
    integer           :: i, k, nz, mZ
    real(real32)      :: ZP, ZSQ, TLG, S, TRM, T1, TST
    real(real32)      :: RLN, FLN, ZM, ZMIN, ZDMY, ZINC
    real(real32)      :: WDTOL
    real(real32)      :: prod, fz

    ! Precomputed lookup table for ln(gamma(n)) for n=1,...,100
    ! Renamed from GLN to gln_table to avoid name conflicts
    real(real32), parameter :: gln_table(100) = [ &
       0.0_real32, 0.0_real32, 6.93147181E-01_real32, 1.79175947E+00_real32, &
       3.17805383E+00_real32, 4.78749174E+00_real32, 6.57925121E+00_real32, 8.52516136E+00_real32, &
       1.06046029E+01_real32, 1.28018275E+01_real32, 1.51044126E+01_real32, 1.75023078E+01_real32, &
       1.99872145E+01_real32, 2.25521639E+01_real32, 2.51912212E+01_real32, 2.78992714E+01_real32, &
       3.06718601E+01_real32, 3.35050735E+01_real32, 3.63954452E+01_real32, 3.93398842E+01_real32, &
       4.23356165E+01_real32, 4.53801389E+01_real32, 4.84711814E+01_real32, 5.16066756E+01_real32, &
       5.47847294E+01_real32, 5.80036052E+01_real32, 6.12617018E+01_real32, 6.45575386E+01_real32, &
       6.78897431E+01_real32, 7.12570390E+01_real32, 7.46582363E+01_real32, 7.80922236E+01_real32, &
       8.15579595E+01_real32, 8.50544670E+01_real32, 8.85808275E+01_real32, 9.21361756E+01_real32, &
       9.57196945E+01_real32, 9.93306125E+01_real32, 1.02968199E+02_real32, 1.06631760E+02_real32, &
       1.10320640E+02_real32, 1.14034212E+02_real32, 1.17771881E+02_real32, 1.21533082E+02_real32, &
       1.25317271E+02_real32, 1.29123934E+02_real32, 1.32952575E+02_real32, 1.36802723E+02_real32, &
       1.40673924E+02_real32, 1.44565744E+02_real32, 1.48477767E+02_real32, 1.52409593E+02_real32, &
       1.56360836E+02_real32, 1.60331128E+02_real32, 1.64320112E+02_real32, 1.68327445E+02_real32, &
       1.72352797E+02_real32, 1.76395848E+02_real32, 1.80456291E+02_real32, 1.84533829E+02_real32, &
       1.88628173E+02_real32, 1.92739047E+02_real32, 1.96866182E+02_real32, 2.01009316E+02_real32, &
       2.05168199E+02_real32, 2.09342587E+02_real32, 2.13532241E+02_real32, 2.17736934E+02_real32, &
       2.21956442E+02_real32, 2.26190548E+02_real32, 2.30439044E+02_real32, 2.34701723E+02_real32, &
       2.38978390E+02_real32, 2.43268849E+02_real32, 2.47572914E+02_real32, 2.51890402E+02_real32, &
       2.56221136E+02_real32, 2.60564941E+02_real32, 2.64921650E+02_real32, 2.69291098E+02_real32, &
       2.73673124E+02_real32, 2.78067573E+02_real32, 2.82474293E+02_real32, 2.86893133E+02_real32, &
       2.91323950E+02_real32, 2.95766601E+02_real32, 3.00220949E+02_real32, 3.04686857E+02_real32, &
       3.09164194E+02_real32, 3.13652830E+02_real32, 3.18152640E+02_real32, 3.22663499E+02_real32, &
       3.27185288E+02_real32, 3.31717887E+02_real32, 3.36261182E+02_real32, 3.40815059E+02_real32, &
       3.45379407E+02_real32, 3.49954118E+02_real32, 3.54539086E+02_real32, 3.59134205E+02_real32 ]

    ! Coefficients for asymptotic expansion (22 coefficients)
    real(real32), parameter :: CF(22) = [ &
       8.33333333E-02_real32, -2.77777778E-03_real32, 7.93650794E-04_real32, &
      -5.95238095E-04_real32, 8.41750842E-04_real32, -1.91752692E-03_real32, &
       6.41025641E-03_real32, -2.95506536E-02_real32, 1.79644372E-01_real32, &
      -1.39243222E+00_real32, 1.34028640E+01_real32, -1.56848285E+02_real32, &
       2.19310333E+03_real32, -3.61087713E+04_real32, 6.91472269E+05_real32, &
      -1.52382215E+07_real32, 3.82900751E+08_real32, -1.08822660E+10_real32, &
       3.47320284E+11_real32, -1.23696021E+13_real32, 4.88788065E+14_real32, &
      -2.13203340E+16_real32 ]

    ! Constant: ln(2*pi)
    real(real32), parameter :: CON = 1.83787707E+00_real32

    ierr = 0
    if (z <= 0.0_real32) then
      ! Return largest positive number R1MACH(2)
      res = r1mach(2)
      ierr = 1
      return
    end if

    ! Use lookup table if z is an exact integer in [1,100] and z <= 101
    if (z <= 101.0_real32) then
      nz = int(z)
      fz = z - real(nz, kind=real32)
      if (fz == 0.0_real32 .and. nz >= 1 .and. nz <= 100) then
        res = gln_table(nz)
        return
      end if
    end if

    ! Set machine tolerance using R1MACH(4)
    WDTOL = r1mach(4)
    if (WDTOL < 0.5E-18_real32) then
      WDTOL = 0.5E-18_real32
    end if

    ! Compute machine-dependent ZMIN threshold using actual machine constants
    ! R1MACH(5) = log10(radix)
    ! I1MACH(11) = number of mantissa digits
    RLN = r1mach(5) * real(i1mach(11), kind=real32)
    FLN = min(RLN, 20.0_real32)
    FLN = max(FLN, 3.0_real32)
    FLN = FLN - 3.0_real32
    ZM = 1.8_real32 + 0.3875_real32 * FLN
    mZ = int(ZM) + 1
    ZMIN = real(mZ, kind=real32)

    ZDMY = z
    ZINC = 0.0_real32
    if (z < ZMIN) then
      nz = int(z)
      ZINC = ZMIN - real(nz, kind=real32)
      ZDMY = z + ZINC
    end if

    ! Asymptotic expansion for ln(gamma)
    ZP = 1.0_real32 / ZDMY
    T1 = CF(1) * ZP
    S = T1
    if (abs(ZP) >= WDTOL) then
      ZSQ = ZP * ZP
      TST = abs(T1) * WDTOL
      do k = 2, 22
        ZP = ZP * ZSQ
        TRM = CF(k) * ZP
        if (abs(TRM) < TST) exit
        S = S + TRM
      end do
    end if

    if (ZINC == 0.0_real32) then
      TLG = log(z)
      res = z * (TLG - 1.0_real32) + 0.5_real32 * (CON - TLG) + S
    else
      prod = 1.0_real32
      nz = int(ZINC)
      do i = 1, nz
        prod = prod * (z + real(i - 1, kind=real32))
      end do
      TLG = log(ZDMY)
      res = ZDMY * (TLG - 1.0_real32) - log(prod) + 0.5_real32 * (CON - TLG) + S
    end if
  end function gamln

end module gamln_module
