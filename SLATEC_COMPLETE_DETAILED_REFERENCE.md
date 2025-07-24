# SLATEC Function Library - Complete Detailed Reference

*Complete alphabetical listing with full descriptions - All 736 Functions*


## A

### AAAAAA
**Purpose:** SLATEC Common Mathematical Library disclaimer and version
**Source:** `aaaaaa.f`

### ASYIK
**Purpose:** Subsidiary to BESI and BESK
**Source:** `asyik.f`

### ASYJY
**Purpose:** Subsidiary to BESJ and BESY
**Source:** `asyjy.f`

### AVINT
**Purpose:** Integrate a function tabulated at arbitrarily spaced abscissas using overlapping parabolas
**Source:** `avint.f`


## B

### BCRH
**Purpose:** Subsidiary to CBLKTR
**Source:** `bcrh.f`

### BDIFF
**Purpose:** Subsidiary to BSKIN
**Source:** `bdiff.f`

### BESI
**Purpose:** Compute an N member sequence of I Bessel functions I/SUB(ALPHA+K-1)/(X), K=1,...,N or scaled Bessel functions EXP(-X)*I/SUB(ALPHA+K-1)/(X), K=1,...,N for non-negative ALPHA and X
**Source:** `besi.f`

### BESJ
**Purpose:** Compute an N member sequence of J Bessel functions J/SUB(ALPHA+K-1)/(X), K=1,...,N for non-negative ALPHA and X
**Source:** `besj.f`

### BESK
**Purpose:** Implement forward recursion on the three term recursion relation for a sequence of non-negative order Bessel functions K/SUB(FNU+I-1)/(X), or scaled Bessel functions EXP(X)*K/SUB(FNU+I-1)/(X), I=1,...,N for real, positive X and non-negative orders FNU
**Source:** `besk.f`

### BESKNU
**Purpose:** Subsidiary to BESK
**Source:** `besknu.f`

### BESY
**Purpose:** Implement forward recursion on the three term recursion relation for a sequence of non-negative order Bessel functions Y/SUB(FNU+I-1)/(X), I=1,...,N for real, positive X and non-negative orders FNU
**Source:** `besy.f`

### BESYNU
**Purpose:** Subsidiary to BESY
**Source:** `besynu.f`

### BFQAD
**Purpose:** Compute the integral of a product of a function and a derivative of a B-spline
**Source:** `bfqad.f`

### BINT4
**Purpose:** Compute the B-representation of a cubic spline which interpolates given data
**Source:** `bint4.f`

### BINTK
**Purpose:** Compute the B-representation of a spline which interpolates given data
**Source:** `bintk.f`

### BKIAS
**Purpose:** Subsidiary to BSKIN
**Source:** `bkias.f`

### BKISR
**Purpose:** Subsidiary to BSKIN
**Source:** `bkisr.f`

### BKSOL
**Purpose:** Subsidiary to BVSUP
**Source:** `bksol.f`

### BLKTR1
**Purpose:** Subsidiary to BLKTRI
**Source:** `blktr1.f`

### BNDACC
**Purpose:** Compute the LU factorization of a banded matrices using sequential accumulation of rows of the data matrix. Exactly one right-hand side vector is permitted
**Source:** `bndacc.f`

### BNDSOL
**Purpose:** Solve the least squares problem for a banded matrix using sequential accumulation of rows of the data matrix. Exactly one right-hand side vector is permitted
**Source:** `bndsol.f`

### BNFAC
**Purpose:** Subsidiary to BINT4 and BINTK
**Source:** `bnfac.f`

### BNSLV
**Purpose:** Subsidiary to BINT4 and BINTK
**Source:** `bnslv.f`

### BSGQ8
**Purpose:** Subsidiary to BFQAD
**Source:** `bsgq8.f`

### BSKIN
**Purpose:** Compute repeated integrals of the K-zero Bessel function
**Source:** `bskin.f`

### BSPDOC
**Purpose:** Documentation for BSPLINE, a package of subprograms for working with piecewise polynomial functions in B-representation
**Source:** `bspdoc.f`

### BSPDR
**Purpose:** Use the B-representation to construct a divided difference table preparatory to a (right) derivative calculation
**Source:** `bspdr.f`

### BSPEV
**Purpose:** Calculate the value of the spline and its derivatives from the B-representation
**Source:** `bspev.f`

### BSPLVD
**Purpose:** Subsidiary to FC
**Source:** `bsplvd.f`

### BSPLVN
**Purpose:** Subsidiary to FC
**Source:** `bsplvn.f`

### BSPPP
**Purpose:** Convert the B-representation of a B-spline to the piecewise polynomial (PP) form
**Source:** `bsppp.f`

### BSPVD
**Purpose:** Calculate the value and all derivatives of order less than NDERIV of all basis functions which do not vanish at X
**Source:** `bspvd.f`

### BSPVN
**Purpose:** Calculate the value of all (possibly) nonzero basis functions at X
**Source:** `bspvn.f`

### BSQAD
**Purpose:** Compute the integral of a K-th order B-spline using the B-representation
**Source:** `bsqad.f`

### BSRH
**Purpose:** Subsidiary to BLKTRI
**Source:** `bsrh.f`

### BVALU
**Purpose:** Evaluate the B-representation of a B-spline at X for the function value or any of its derivatives
**Source:** `bvalu.f`

### BVDER
**Purpose:** Subsidiary to BVSUP
**Source:** `bvder.f`

### BVPOR
**Purpose:** Subsidiary to BVSUP
**Source:** `bvpor.f`

### BVSUP
**Purpose:** Solve a linear two-point boundary value problem using superposition coupled with an orthonormalization procedure and a variable-step integration scheme
**Source:** `bvsup.f`


## C

### C1MERG
**Purpose:** Merge two strings of complex numbers. Each string is ascending by the real part
**Source:** `c1merg.f`

### CACAI
**Purpose:** Subsidiary to CAIRY
**Source:** `cacai.f`

### CACON
**Purpose:** Subsidiary to CBESH and CBESK
**Source:** `cacon.f`

### CAIRY
**Purpose:** Compute the Airy function Ai(z) or its derivative dAi/dz for complex argument z. A scaling option is available to help avoid underflow and overflow
**Source:** `cairy.f`

### CASYI
**Purpose:** Subsidiary to CBESI and CBESK
**Source:** `casyi.f`

### CBESH
**Purpose:** Compute a sequence of the Hankel functions H(m,a,z) for superscript m=1 or 2, real nonnegative orders a=b, b+1,... where b>0, and nonzero complex argument z. A scaling option is available to help avoid overflow
**Source:** `cbesh.f`

### CBESI
**Purpose:** Compute a sequence of the Bessel functions I(a,z) for complex argument z and real nonnegative orders a=b,b+1, b+2,... where b>0. A scaling option is available to help avoid overflow
**Source:** `cbesi.f`

### CBESJ
**Purpose:** Compute a sequence of the Bessel functions J(a,z) for complex argument z and real nonnegative orders a=b,b+1, b+2,... where b>0. A scaling option is available to help avoid overflow
**Source:** `cbesj.f`

### CBESK
**Purpose:** Compute a sequence of the Bessel functions K(a,z) for complex argument z and real nonnegative orders a=b,b+1, b+2,... where b>0. A scaling option is available to help avoid overflow
**Source:** `cbesk.f`

### CBESY
**Purpose:** Compute a sequence of the Bessel functions Y(a,z) for complex argument z and real nonnegative orders a=b,b+1, b+2,... where b>0. A scaling option is available to help avoid overflow
**Source:** `cbesy.f`

### CBINU
**Purpose:** Subsidiary to CAIRY, CBESH, CBESI, CBESJ, CBESK and CBIRY
**Source:** `cbinu.f`

### CBIRY
**Purpose:** Compute the Airy function Bi(z) or its derivative dBi/dz for complex argument z. A scaling option is available to help avoid overflow
**Source:** `cbiry.f`

### CBKNU
**Purpose:** Subsidiary to CAIRY, CBESH, CBESI and CBESK
**Source:** `cbknu.f`

### CBLKT1
**Purpose:** Subsidiary to CBLKTR
**Source:** `cblkt1.f`

### CBUNI
**Purpose:** Subsidiary to CBESI and CBESK
**Source:** `cbuni.f`

### CBUNK
**Purpose:** Subsidiary to CBESH and CBESK
**Source:** `cbunk.f`

### CCMPB
**Purpose:** Subsidiary to CBLKTR
**Source:** `ccmpb.f`

### CDCOR
**Purpose:** Subroutine CDCOR computes corrections to the Y array
**Source:** `cdcor.f`

### CDCST
**Purpose:** CDCST sets coefficients used by the core integrator CDSTP
**Source:** `cdcst.f`

### CDIV
**Purpose:** Compute the complex quotient of two complex numbers
**Source:** `cdiv.f`

### CDNTL
**Purpose:** Subroutine CDNTL is called to set parameters on the first call to CDSTP, on an internal restart, or when the user has altered MINT, MITER, and/or H
**Source:** `cdntl.f`

### CDNTP
**Purpose:** Subroutine CDNTP interpolates the K-th derivative of Y at TOUT, using the data in the YH array. If K has a value greater than NQ, the NQ-th derivative is calculated
**Source:** `cdntp.f`

### CDPSC
**Purpose:** Subroutine CDPSC computes the predicted YH values by effectively multiplying the YH array by the Pascal triangle matrix when KSGN is +1, and performs the inverse function when KSGN is -1
**Source:** `cdpsc.f`

### CDPST
**Purpose:** Subroutine CDPST evaluates the Jacobian matrix of the right hand side of the differential equations
**Source:** `cdpst.f`

### CDRIV1
**Purpose:** The function of CDRIV1 is to solve N (200 or fewer) ordinary differential equations of the form dY(I)/dT = F(Y(I),T), given the initial conditions Y(I) = YI. CDRIV1 allows complex-valued differential equations
**Source:** `cdriv1.f`

### CDRIV2
**Purpose:** The function of CDRIV2 is to solve N ordinary differential equations of the form dY(I)/dT = F(Y(I),T), given the initial conditions Y(I) = YI. The program has options to allow the solution of both stiff and non-stiff differential equations. CDRIV2 allows complex-valued differential equations
**Source:** `cdriv2.f`

### CDRIV3
**Purpose:** The function of CDRIV3 is to solve N ordinary differential equations of the form dY(I)/dT = F(Y(I),T), given the initial conditions Y(I) = YI. The program has options to allow the solution of both stiff and non-stiff differential equations. Other important options are available. CDRIV3 allows complex-valued differential equations
**Source:** `cdriv3.f`

### CDSCL
**Purpose:** Subroutine CDSCL rescales the YH array whenever the step size is changed
**Source:** `cdscl.f`

### CDSTP
**Purpose:** CDSTP performs one step of the integration of an initial value problem for a system of ordinary differential equations
**Source:** `cdstp.f`

### CDZRO
**Purpose:** CDZRO searches for a zero of a function F(N, T, Y, IROOT) between the given values B and C until the width of the interval (B, C) has collapsed to within a tolerance specified by the stopping criterion, ABS(B - C) .LE. 2.*(RW*ABS(B) + AE)
**Source:** `cdzro.f`

### CFOD
**Purpose:** Subsidiary to DEBDF
**Source:** `cfod.f`

### CGEEV
**Purpose:** Compute the eigenvalues and, optionally, the eigenvectors of a complex general matrix
**Source:** `cgeev.f`

### CGEFS
**Purpose:** Solve a general system of linear equations
**Source:** `cgefs.f`

### CGEIR
**Purpose:** Solve a general system of linear equations. Iterative refinement is used to obtain an error estimate
**Source:** `cgeir.f`

### CHIEV
**Purpose:** Compute the eigenvalues and, optionally, the eigenvectors of a complex Hermitian matrix
**Source:** `chiev.f`

### CHKDER
**Purpose:** Check the gradients of M nonlinear functions in N variables, evaluated at a point X, for consistency with the functions themselves
**Source:** `chkder.f`

### CHKPR4
**Purpose:** Subsidiary to SEPX4
**Source:** `chkpr4.f`

### CHKPRM
**Purpose:** Subsidiary to SEPELI
**Source:** `chkprm.f`

### CHKSN4
**Purpose:** Subsidiary to SEPX4
**Source:** `chksn4.f`

### CHKSNG
**Purpose:** Subsidiary to SEPELI
**Source:** `chksng.f`

### CKSCL
**Purpose:** Subsidiary to CBKNU, CUNK1 and CUNK2
**Source:** `ckscl.f`

### CMLRI
**Purpose:** Subsidiary to CBESI and CBESK
**Source:** `cmlri.f`

### CMPCSG
**Purpose:** Subsidiary to CMGNBN
**Source:** `cmpcsg.f`

### CMPOSD
**Purpose:** Subsidiary to CMGNBN
**Source:** `cmposd.f`

### CMPOSN
**Purpose:** Subsidiary to CMGNBN
**Source:** `cmposn.f`

### CMPOSP
**Purpose:** Subsidiary to CMGNBN
**Source:** `cmposp.f`

### CMPTR3
**Purpose:** Subsidiary to CMGNBN
**Source:** `cmptr3.f`

### CMPTRX
**Purpose:** Subsidiary to CMGNBN
**Source:** `cmptrx.f`

### CNBCO
**Purpose:** Factor a band matrix using Gaussian elimination and estimate the condition number
**Source:** `cnbco.f`

### CNBDI
**Purpose:** Compute the determinant of a band matrix using the factors computed by CNBCO or CNBFA
**Source:** `cnbdi.f`

### CNBFA
**Purpose:** Factor a band matrix by elimination
**Source:** `cnbfa.f`

### CNBFS
**Purpose:** Solve a general nonsymmetric banded system of linear equations
**Source:** `cnbfs.f`

### CNBIR
**Purpose:** Solve a general nonsymmetric banded system of linear equations. Iterative refinement is used to obtain an error estimate
**Source:** `cnbir.f`

### CNBSL
**Purpose:** Solve a complex band system using the factors computed by CNBCO or CNBFA
**Source:** `cnbsl.f`

### COMPB
**Purpose:** Subsidiary to BLKTRI
**Source:** `compb.f`

### COSGEN
**Purpose:** Subsidiary to GENBUN
**Source:** `cosgen.f`

### CPADD
**Purpose:** Subsidiary to CBLKTR
**Source:** `cpadd.f`

### CPEVL
**Purpose:** Subsidiary to CPZERO
**Source:** `cpevl.f`

### CPEVLR
**Purpose:** Subsidiary to CPZERO
**Source:** `cpevlr.f`

### CPOFS
**Purpose:** Solve a positive definite symmetric complex system of linear equations
**Source:** `cpofs.f`

### CPOIR
**Purpose:** Solve a positive definite Hermitian system of linear equations. Iterative refinement is used to obtain an error estimate
**Source:** `cpoir.f`

### CPQR79
**Purpose:** Find the zeros of a polynomial with complex coefficients
**Source:** `cpqr79.f`

### CPROC
**Purpose:** Subsidiary to CBLKTR
**Source:** `cproc.f`

### CPROCP
**Purpose:** Subsidiary to CBLKTR
**Source:** `cprocp.f`

### CPROD
**Purpose:** Subsidiary to BLKTRI
**Source:** `cprod.f`

### CPRODP
**Purpose:** Subsidiary to BLKTRI
**Source:** `cprodp.f`

### CPZERO
**Purpose:** Find the zeros of a polynomial with complex coefficients
**Source:** `cpzero.f`

### CRATI
**Purpose:** Subsidiary to CBESH, CBESI and CBESK
**Source:** `crati.f`

### CS1S2
**Purpose:** Subsidiary to CAIRY and CBESK
**Source:** `cs1s2.f`

### CSCALE
**Purpose:** Subsidiary to BVSUP
**Source:** `cscale.f`

### CSERI
**Purpose:** Subsidiary to CBESI and CBESK
**Source:** `cseri.f`

### CSHCH
**Purpose:** Subsidiary to CBESH and CBESK
**Source:** `cshch.f`

### CSROOT
**Purpose:** Compute the complex square root of a complex number
**Source:** `csroot.f`

### CUCHK
**Purpose:** Subsidiary to SERI, CUOIK, CUNK1, CUNK2, CUNI1, CUNI2 and CKSCL
**Source:** `cuchk.f`

### CUNHJ
**Purpose:** Subsidiary to CBESI and CBESK
**Source:** `cunhj.f`

### CUNI1
**Purpose:** Subsidiary to CBESI and CBESK
**Source:** `cuni1.f`

### CUNI2
**Purpose:** Subsidiary to CBESI and CBESK
**Source:** `cuni2.f`

### CUNIK
**Purpose:** Subsidiary to CBESI and CBESK
**Source:** `cunik.f`

### CUNK1
**Purpose:** Subsidiary to CBESK
**Source:** `cunk1.f`

### CUNK2
**Purpose:** Subsidiary to CBESK
**Source:** `cunk2.f`

### CUOIK
**Purpose:** Subsidiary to CBESH, CBESI and CBESK
**Source:** `cuoik.f`

### CV
**Purpose:** Evaluate the variance function of the curve obtained by the constrained B-spline fitting subprogram FC
**Source:** `cv.f`

### CWRSK
**Purpose:** Subsidiary to CBESI and CBESK
**Source:** `cwrsk.f`


## D

### D1MACH
**Purpose:** Return floating point machine dependent constants
**Source:** `d1mach_ieee.f`

### D1MERG
**Purpose:** Merge two strings of ascending double precision numbers
**Source:** `d1merg.f`

### D1MPYQ
**Purpose:** Subsidiary to DNSQ and DNSQE
**Source:** `d1mpyq.f`

### D1UPDT
**Purpose:** Subsidiary to DNSQ and DNSQE
**Source:** `d1updt.f`

### DASYIK
**Purpose:** Subsidiary to DBESI and DBESK
**Source:** `dasyik.f`

### DASYJY
**Purpose:** Subsidiary to DBESJ and DBESY
**Source:** `dasyjy.f`

### DAVINT
**Purpose:** Integrate a function tabulated at arbitrarily spaced abscissas using overlapping parabolas
**Source:** `davint.f`

### DBDIFF
**Purpose:** Subsidiary to DBSKIN
**Source:** `dbdiff.f`

### DBESI
**Purpose:** Compute an N member sequence of I Bessel functions I/SUB(ALPHA+K-1)/(X), K=1,...,N or scaled Bessel functions EXP(-X)*I/SUB(ALPHA+K-1)/(X), K=1,...,N for nonnegative ALPHA and X
**Source:** `dbesi.f`

### DBESJ
**Purpose:** Compute an N member sequence of J Bessel functions J/SUB(ALPHA+K-1)/(X), K=1,...,N for non-negative ALPHA and X
**Source:** `dbesj.f`

### DBESK
**Purpose:** Implement forward recursion on the three term recursion relation for a sequence of non-negative order Bessel functions K/SUB(FNU+I-1)/(X), or scaled Bessel functions EXP(X)*K/SUB(FNU+I-1)/(X), I=1,...,N for real, positive X and non-negative orders FNU
**Source:** `dbesk.f`

### DBESY
**Purpose:** Implement forward recursion on the three term recursion relation for a sequence of non-negative order Bessel functions Y/SUB(FNU+I-1)/(X), I=1,...,N for real, positive X and non-negative orders FNU
**Source:** `dbesy.f`

### DBFQAD
**Purpose:** Compute the integral of a product of a function and a derivative of a K-th order B-spline
**Source:** `dbfqad.f`

### DBINT4
**Purpose:** Compute the B-representation of a cubic spline which interpolates given data
**Source:** `dbint4.f`

### DBINTK
**Purpose:** Compute the B-representation of a spline which interpolates given data
**Source:** `dbintk.f`

### DBKIAS
**Purpose:** Subsidiary to DBSKIN
**Source:** `dbkias.f`

### DBKISR
**Purpose:** Subsidiary to DBSKIN
**Source:** `dbkisr.f`

### DBKSOL
**Purpose:** Subsidiary to DBVSUP
**Source:** `dbksol.f`

### DBNDAC
**Purpose:** Compute the LU factorization of a banded matrices using sequential accumulation of rows of the data matrix. Exactly one right-hand side vector is permitted
**Source:** `dbndac.f`

### DBNDSL
**Purpose:** Solve the least squares problem for a banded matrix using sequential accumulation of rows of the data matrix. Exactly one right-hand side vector is permitted
**Source:** `dbndsl.f`

### DBNFAC
**Purpose:** Subsidiary to DBINT4 and DBINTK
**Source:** `dbnfac.f`

### DBNSLV
**Purpose:** Subsidiary to DBINT4 and DBINTK
**Source:** `dbnslv.f`

### DBOCLS
**Purpose:** Solve the bounded and constrained least squares problem consisting of solving the equation E*X = F (in the least squares sense) subject to the linear constraints C*X = Y
**Source:** `dbocls.f`

### DBOLS
**Purpose:** Solve the problem E*X = F (in the least squares sense) with bounds on selected X values
**Source:** `dbols.f`

### DBOLSM
**Purpose:** Subsidiary to DBOCLS and DBOLS
**Source:** `dbolsm.f`

### DBSGQ8
**Purpose:** Subsidiary to DBFQAD
**Source:** `dbsgq8.f`

### DBSKIN
**Purpose:** Compute repeated integrals of the K-zero Bessel function
**Source:** `dbskin.f`

### DBSKNU
**Purpose:** Subsidiary to DBESK
**Source:** `dbsknu.f`

### DBSPDR
**Purpose:** Use the B-representation to construct a divided difference table preparatory to a (right) derivative calculation
**Source:** `dbspdr.f`

### DBSPEV
**Purpose:** Calculate the value of the spline and its derivatives from the B-representation
**Source:** `dbspev.f`

### DBSPPP
**Purpose:** Convert the B-representation of a B-spline to the piecewise polynomial (PP) form
**Source:** `dbsppp.f`

### DBSPVD
**Purpose:** Calculate the value and all derivatives of order less than NDERIV of all basis functions which do not vanish at X
**Source:** `dbspvd.f`

### DBSPVN
**Purpose:** Calculate the value of all (possibly) nonzero basis functions at X
**Source:** `dbspvn.f`

### DBSQAD
**Purpose:** Compute the integral of a K-th order B-spline using the B-representation
**Source:** `dbsqad.f`

### DBSYNU
**Purpose:** Subsidiary to DBESY
**Source:** `dbsynu.f`

### DBVALU
**Purpose:** Evaluate the B-representation of a B-spline at X for the function value or any of its derivatives
**Source:** `dbvalu.f`

### DBVDER
**Purpose:** Subsidiary to DBVSUP
**Source:** `dbvder.f`

### DBVPOR
**Purpose:** Subsidiary to DBVSUP
**Source:** `dbvpor.f`

### DBVSUP
**Purpose:** Solve a linear two-point boundary value problem using superposition coupled with an orthonormalization procedure and a variable-step integration scheme
**Source:** `dbvsup.f`

### DCFOD
**Purpose:** Subsidiary to DDEBDF
**Source:** `dcfod.f`

### DCKDER
**Purpose:** Check the gradients of M nonlinear functions in N variables, evaluated at a point X, for consistency with the functions themselves
**Source:** `dckder.f`

### DCOEF
**Purpose:** Subsidiary to DBVSUP
**Source:** `dcoef.f`

### DCOV
**Purpose:** Calculate the covariance matrix for a nonlinear data fitting problem. It is intended to be used after a successful return from either DNLS1 or DNLS1E
**Source:** `dcov.f`

### DCSCAL
**Purpose:** Subsidiary to DBVSUP and DSUDS
**Source:** `dcscal.f`

### DCV
**Purpose:** Evaluate the variance function of the curve obtained by the constrained B-spline fitting subprogram DFC
**Source:** `dcv.f`

### DDAINI
**Purpose:** Initialization routine for DDASSL
**Source:** `ddaini.f`

### DDAJAC
**Purpose:** Compute the iteration matrix for DDASSL and form the LU-decomposition
**Source:** `ddajac.f`

### DDANRM
**Purpose:** Compute vector norm for DDASSL
**Source:** `ddanrm.f`

### DDASLV
**Purpose:** Linear system solver for DDASSL
**Source:** `ddaslv.f`

### DDASSL
**Purpose:** This code solves a system of differential/algebraic equations of the form G(T,Y,YPRIME) = 0
**Source:** `ddassl.f`

### DDASTP
**Purpose:** Perform one step of the DDASSL integration
**Source:** `ddastp.f`

### DDATRP
**Purpose:** Interpolation routine for DDASSL
**Source:** `ddatrp.f`

### DDAWTS
**Purpose:** Set error weight vector for DDASSL
**Source:** `ddawts.f`

### DDCOR
**Purpose:** Subroutine DDCOR computes corrections to the Y array
**Source:** `ddcor.f`

### DDCST
**Purpose:** DDCST sets coefficients used by the core integrator DDSTP
**Source:** `ddcst.f`

### DDEABM
**Purpose:** Solve an initial value problem in ordinary differential equations using an Adams-Bashforth method
**Source:** `ddeabm.f`

### DDEBDF
**Purpose:** Solve an initial value problem in ordinary differential equations using backward differentiation formulas. It is intended primarily for stiff problems
**Source:** `ddebdf.f`

### DDERKF
**Purpose:** Solve an initial value problem in ordinary differential equations using a Runge-Kutta-Fehlberg scheme
**Source:** `dderkf.f`

### DDES
**Purpose:** Subsidiary to DDEABM
**Source:** `ddes.f`

### DDNTL
**Purpose:** Subroutine DDNTL is called to set parameters on the first call to DDSTP, on an internal restart, or when the user has altered MINT, MITER, and/or H
**Source:** `ddntl.f`

### DDNTP
**Purpose:** Subroutine DDNTP interpolates the K-th derivative of Y at TOUT, using the data in the YH array. If K has a value greater than NQ, the NQ-th derivative is calculated
**Source:** `ddntp.f`

### DDOGLG
**Purpose:** Subsidiary to DNSQ and DNSQE
**Source:** `ddoglg.f`

### DDPSC
**Purpose:** Subroutine DDPSC computes the predicted YH values by effectively multiplying the YH array by the Pascal triangle matrix when KSGN is +1, and performs the inverse function when KSGN is -1
**Source:** `ddpsc.f`

### DDPST
**Purpose:** Subroutine DDPST evaluates the Jacobian matrix of the right hand side of the differential equations
**Source:** `ddpst.f`

### DDRIV1
**Purpose:** The function of DDRIV1 is to solve N (200 or fewer) ordinary differential equations of the form dY(I)/dT = F(Y(I),T), given the initial conditions Y(I) = YI. DDRIV1 uses double precision arithmetic
**Source:** `ddriv1.f`

### DDRIV2
**Purpose:** The function of DDRIV2 is to solve N ordinary differential equations of the form dY(I)/dT = F(Y(I),T), given the initial conditions Y(I) = YI. The program has options to allow the solution of both stiff and non-stiff differential equations. DDRIV2 uses double precision arithmetic
**Source:** `ddriv2.f`

### DDRIV3
**Purpose:** The function of DDRIV3 is to solve N ordinary differential equations of the form dY(I)/dT = F(Y(I),T), given the initial conditions Y(I) = YI. The program has options to allow the solution of both stiff and non-stiff differential equations. Other important options are available. DDRIV3 uses double precision arithmetic
**Source:** `ddriv3.f`

### DDSCL
**Purpose:** Subroutine DDSCL rescales the YH array whenever the step size is changed
**Source:** `ddscl.f`

### DDSTP
**Purpose:** DDSTP performs one step of the integration of an initial value problem for a system of ordinary differential equations
**Source:** `ddstp.f`

### DDZRO
**Purpose:** DDZRO searches for a zero of a function F(N, T, Y, IROOT) between the given values B and C until the width of the interval (B, C) has collapsed to within a tolerance specified by the stopping criterion, ABS(B - C) .LE. 2.*(RW*ABS(B) + AE)
**Source:** `ddzro.f`

### DEABM
**Purpose:** Solve an initial value problem in ordinary differential equations using an Adams-Bashforth method
**Source:** `deabm.f`

### DEBDF
**Purpose:** Solve an initial value problem in ordinary differential equations using backward differentiation formulas. It is intended primarily for stiff problems
**Source:** `debdf.f`

### DEFC
**Purpose:** Fit a piecewise polynomial curve to discrete data. The piecewise polynomials are represented as B-splines. The fitting is done in a weighted least squares sense
**Source:** `defc.f`

### DEFCMN
**Purpose:** Subsidiary to DEFC
**Source:** `defcmn.f`

### DEFE4
**Purpose:** Subsidiary to SEPX4
**Source:** `defe4.f`

### DEFEHL
**Purpose:** Subsidiary to DERKF
**Source:** `defehl.f`

### DEFER
**Purpose:** Subsidiary to SEPELI
**Source:** `defer.f`

### DENORM
**Purpose:** Subsidiary to DNSQ and DNSQE
**Source:** `denorm.f`

### DERKF
**Purpose:** Solve an initial value problem in ordinary differential equations using a Runge-Kutta-Fehlberg scheme
**Source:** `derkf.f`

### DERKFS
**Purpose:** Subsidiary to DERKF
**Source:** `derkfs.f`

### DES
**Purpose:** Subsidiary to DEABM
**Source:** `des.f`

### DEXBVP
**Purpose:** Subsidiary to DBVSUP
**Source:** `dexbvp.f`

### DEXINT
**Purpose:** Compute an M member sequence of exponential integrals E(N+K,X), K=0,1,...,M-1 for N .GE. 1 and X .GE. 0
**Source:** `dexint.f`

### DFC
**Purpose:** Fit a piecewise polynomial curve to discrete data. The piecewise polynomials are represented as B-splines. The fitting is done in a weighted least squares sense. Equality and inequality constraints can be imposed on the fitted curve
**Source:** `dfc.f`

### DFCMN
**Purpose:** Subsidiary to FC
**Source:** `dfcmn.f`

### DFDJC1
**Purpose:** Subsidiary to DNSQ and DNSQE
**Source:** `dfdjc1.f`

### DFDJC3
**Purpose:** Subsidiary to DNLS1 and DNLS1E
**Source:** `dfdjc3.f`

### DFEHL
**Purpose:** Subsidiary to DDERKF
**Source:** `dfehl.f`

### DFSPVD
**Purpose:** Subsidiary to DFC
**Source:** `dfspvd.f`

### DFSPVN
**Purpose:** Subsidiary to DFC
**Source:** `dfspvn.f`

### DFULMT
**Purpose:** Subsidiary to DSPLP
**Source:** `dfulmt.f`

### DFZERO
**Purpose:** Search for a zero of a function F(X) in a given interval (B,C). It is designed primarily for problems where F(B) and F(C) have opposite signs
**Source:** `dfzero.f`

### DGAMLN
**Purpose:** Compute the logarithm of the Gamma function
**Source:** `dgamln.f`

### DGAMRN
**Purpose:** Subsidiary to DBSKIN
**Source:** `dgamrn.f`

### DGAUS8
**Purpose:** Integrate a real function of one variable over a finite interval using an adaptive 8-point Legendre-Gauss algorithm. Intended primarily for high accuracy integration or integration of smooth functions
**Source:** `dgaus8.f`

### DGEFS
**Purpose:** Solve a general system of linear equations
**Source:** `dgefs.f`

### DGLSS
**Purpose:** Solve a linear least squares problems by performing a QR factorization of the input matrix using Householder transformations. Emphasis is put on detecting possible rank deficiency
**Source:** `dglss.f`

### DH12
**Purpose:** Subsidiary to DHFTI, DLSEI and DWNNLS
**Source:** `dh12.f`

### DHFTI
**Purpose:** Solve a least squares problem for banded matrices using sequential accumulation of rows of the data matrix. Exactly one right-hand side vector is permitted
**Source:** `dhfti.f`

### DHKSEQ
**Purpose:** Subsidiary to DBSKIN
**Source:** `dhkseq.f`

### DHSTRT
**Purpose:** Subsidiary to DDEABM, DDEBDF and DDERKF
**Source:** `dhstrt.f`

### DHVNRM
**Purpose:** Subsidiary to DDEABM, DDEBDF and DDERKF
**Source:** `dhvnrm.f`

### DINTP
**Purpose:** Approximate the solution at XOUT by evaluating the polynomial computed in DSTEPS at XOUT. Must be used in conjunction with DSTEPS
**Source:** `dintp.f`

### DINTRV
**Purpose:** Compute the largest integer ILEFT in 1 .LE. ILEFT .LE. LXT such that XT(ILEFT) .LE. X where XT(*) is a subdivision of the X interval
**Source:** `dintrv.f`

### DINTYD
**Purpose:** Subsidiary to DDEBDF
**Source:** `dintyd.f`

### DJAIRY
**Purpose:** Subsidiary to DBESJ and DBESY
**Source:** `djairy.f`

### DLLSIA
**Purpose:** Solve linear least squares problems by performing a QR factorization of the input matrix using Householder transformations. Emphasis is put on detecting possible rank deficiency
**Source:** `dllsia.f`

### DLPDP
**Purpose:** Subsidiary to DLSEI
**Source:** `dlpdp.f`

### DLSEI
**Purpose:** Solve a linearly constrained least squares problem with equality and inequality constraints, and optionally compute a covariance matrix
**Source:** `dlsei.f`

### DLSI
**Purpose:** Subsidiary to DLSEI
**Source:** `dlsi.f`

### DLSOD
**Purpose:** Subsidiary to DDEBDF
**Source:** `dlsod.f`

### DLSSUD
**Purpose:** Subsidiary to DBVSUP and DSUDS
**Source:** `dlssud.f`

### DMACON
**Purpose:** Subsidiary to DBVSUP
**Source:** `dmacon.f`

### DMGSBV
**Purpose:** Subsidiary to DBVSUP
**Source:** `dmgsbv.f`

### DMOUT
**Purpose:** Subsidiary to DBOCLS and DFC
**Source:** `dmout.f`

### DMPAR
**Purpose:** Subsidiary to DNLS1 and DNLS1E
**Source:** `dmpar.f`

### DNBCO
**Purpose:** Factor a band matrix using Gaussian elimination and estimate the condition number
**Source:** `dnbco.f`

### DNBDI
**Purpose:** Compute the determinant of a band matrix using the factors computed by DNBCO or DNBFA
**Source:** `dnbdi.f`

### DNBFA
**Purpose:** Factor a band matrix by elimination
**Source:** `dnbfa.f`

### DNBFS
**Purpose:** Solve a general nonsymmetric banded system of linear equations
**Source:** `dnbfs.f`

### DNBSL
**Purpose:** Solve a real band system using the factors computed by DNBCO or DNBFA
**Source:** `dnbsl.f`

### DNLS1
**Purpose:** Minimize the sum of the squares of M nonlinear functions in N variables by a modification of the Levenberg-Marquardt algorithm
**Source:** `dnls1.f`

### DNLS1E
**Purpose:** An easy-to-use code which minimizes the sum of the squares of M nonlinear functions in N variables by a modification of the Levenberg-Marquardt algorithm
**Source:** `dnls1e.f`

### DNSQ
**Purpose:** Find a zero of a system of a N nonlinear functions in N variables by a modification of the Powell hybrid method
**Source:** `dnsq.f`

### DNSQE
**Purpose:** An easy-to-use code to find a zero of a system of N nonlinear functions in N variables by a modification of the Powell hybrid method
**Source:** `dnsqe.f`

### DOGLEG
**Purpose:** Subsidiary to SNSQ and SNSQE
**Source:** `dogleg.f`

### DOHTRL
**Purpose:** Subsidiary to DBVSUP and DSUDS
**Source:** `dohtrl.f`

### DORTHR
**Purpose:** Subsidiary to DBVSUP and DSUDS
**Source:** `dorthr.f`

### DP1VLU
**Purpose:** Use the coefficients generated by DPOLFT to evaluate the polynomial fit of degree L, along with the first NDER of its derivatives, at a specified point
**Source:** `dp1vlu.f`

### DPCHNG
**Purpose:** Subsidiary to DSPLP
**Source:** `dpchng.f`

### DPCOEF
**Purpose:** Convert the DPOLFT coefficients to Taylor series form
**Source:** `dpcoef.f`

### DPFQAD
**Purpose:** Compute the integral on (X1,X2) of a product of a function F and the ID-th derivative of a B-spline, (PP-representation)
**Source:** `dpfqad.f`

### DPINCW
**Purpose:** Subsidiary to DSPLP
**Source:** `dpincw.f`

### DPINIT
**Purpose:** Subsidiary to DSPLP
**Source:** `dpinit.f`

### DPINTM
**Purpose:** Subsidiary to DSPLP
**Source:** `dpintm.f`

### DPJAC
**Purpose:** Subsidiary to DDEBDF
**Source:** `dpjac.f`

### DPLINT
**Purpose:** Produce the polynomial which interpolates a set of discrete data points
**Source:** `dplint.f`

### DPLPCE
**Purpose:** Subsidiary to DSPLP
**Source:** `dplpce.f`

### DPLPDM
**Purpose:** Subsidiary to DSPLP
**Source:** `dplpdm.f`

### DPLPFE
**Purpose:** Subsidiary to DSPLP
**Source:** `dplpfe.f`

### DPLPFL
**Purpose:** Subsidiary to DSPLP
**Source:** `dplpfl.f`

### DPLPMN
**Purpose:** Subsidiary to DSPLP
**Source:** `dplpmn.f`

### DPLPMU
**Purpose:** Subsidiary to DSPLP
**Source:** `dplpmu.f`

### DPLPUP
**Purpose:** Subsidiary to DSPLP
**Source:** `dplpup.f`

### DPNNZR
**Purpose:** Subsidiary to DSPLP
**Source:** `dpnnzr.f`

### DPOFS
**Purpose:** Solve a positive definite symmetric system of linear equations
**Source:** `dpofs.f`

### DPOLCF
**Purpose:** Compute the coefficients of the polynomial fit (including Hermite polynomial fits) produced by a previous call to POLINT
**Source:** `dpolcf.f`

### DPOLFT
**Purpose:** Fit discrete data in a least squares sense by polynomials in one variable
**Source:** `dpolft.f`

### DPOLVL
**Purpose:** Calculate the value of a polynomial and its first NDER derivatives where the polynomial was produced by a previous call to DPLINT
**Source:** `dpolvl.f`

### DPOPT
**Purpose:** Subsidiary to DSPLP
**Source:** `dpopt.f`

### DPPERM
**Purpose:** Rearrange a given array according to a prescribed permutation vector
**Source:** `dpperm.f`

### DPPGQ8
**Purpose:** Subsidiary to DPFQAD
**Source:** `dppgq8.f`

### DPPQAD
**Purpose:** Compute the integral on (X1,X2) of a K-th order B-spline using the piecewise polynomial (PP) representation
**Source:** `dppqad.f`

### DPPVAL
**Purpose:** Calculate the value of the IDERIV-th derivative of the B-spline from the PP-representation
**Source:** `dppval.f`

### DPRVEC
**Purpose:** Subsidiary to DBVSUP
**Source:** `dprvec.f`

### DPRWPG
**Purpose:** Subsidiary to DSPLP
**Source:** `dprwpg.f`

### DPRWVR
**Purpose:** Subsidiary to DSPLP
**Source:** `dprwvr.f`

### DPSIFN
**Purpose:** Compute derivatives of the Psi function
**Source:** `dpsifn.f`

### DPSIXN
**Purpose:** Subsidiary to DEXINT
**Source:** `dpsixn.f`

### DPSORT
**Purpose:** Return the permutation vector generated by sorting a given array and, optionally, rearrange the elements of the array. The array may be sorted in increasing or decreasing order. A slightly modified quicksort algorithm is used
**Source:** `dpsort.f`

### DQAG
**Purpose:** The routine calculates an approximation result to a given definite integral I = integral of F over (A,B), hopefully satisfying following claim for accuracy ABS(I-RESULT)LE.MAX(EPSABS,EPSREL*ABS(I))
**Source:** `dqag.f`

### DQAGE
**Purpose:** The routine calculates an approximation result to a given definite integral I = Integral of F over (A,B), hopefully satisfying following claim for accuracy ABS(I-RESLT).LE.MAX(EPSABS,EPSREL*ABS(I))
**Source:** `dqage.f`

### DQAGI
**Purpose:** The routine calculates an approximation result to a given INTEGRAL I = Integral of F over (BOUND,+INFINITY) OR I = Integral of F over (-INFINITY,BOUND) OR I = Integral of F over (-INFINITY,+INFINITY) Hopefully satisfying following claim for accuracy ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I))
**Source:** `dqagi.f`

### DQAGIE
**Purpose:** The routine calculates an approximation result to a given integral I = Integral of F over (BOUND,+INFINITY) or I = Integral of F over (-INFINITY,BOUND) or I = Integral of F over (-INFINITY,+INFINITY), hopefully satisfying following claim for accuracy ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I))
**Source:** `dqagie.f`

### DQAGP
**Purpose:** The routine calculates an approximation result to a given definite integral I = Integral of F over (A,B), hopefully satisfying following claim for accuracy break points of the integration interval, where local difficulties of the integrand may occur (e.g. SINGULARITIES, DISCONTINUITIES), are provided by the user
**Source:** `dqagp.f`

### DQAGPE
**Purpose:** Approximate a given definite integral I = Integral of F over (A,B), hopefully satisfying the accuracy claim: ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I)). Break points of the integration interval, where local difficulties of the integrand may occur (e.g. singularities or discontinuities) are provided by the user
**Source:** `dqagpe.f`

### DQAGS
**Purpose:** The routine calculates an approximation result to a given Definite integral I = Integral of F over (A,B), Hopefully satisfying following claim for accuracy ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I))
**Source:** `dqags.f`

### DQAGSE
**Purpose:** The routine calculates an approximation result to a given definite integral I = Integral of F over (A,B), hopefully satisfying following claim for accuracy ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I))
**Source:** `dqagse.f`

### DQAWC
**Purpose:** The routine calculates an approximation result to a Cauchy principal value I = INTEGRAL of F*W over (A,B) (W(X) = 1/((X-C), C.NE.A, C.NE.B), hopefully satisfying following claim for accuracy ABS(I-RESULT).LE.MAX(EPSABE,EPSREL*ABS(I))
**Source:** `dqawc.f`

### DQAWCE
**Purpose:** The routine calculates an approximation result to a CAUCHY PRINCIPAL VALUE I = Integral of F*W over (A,B) (W(X) = 1/(X-C), (C.NE.A, C.NE.B), hopefully satisfying following claim for accuracy ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I))
**Source:** `dqawce.f`

### DQAWF
**Purpose:** The routine calculates an approximation result to a given Fourier integral I=Integral of F(X)*W(X) over (A,INFINITY) where W(X) = COS(OMEGA*X) or W(X) = SIN(OMEGA*X). Hopefully satisfying following claim for accuracy ABS(I-RESULT).LE.EPSABS
**Source:** `dqawf.f`

### DQAWFE
**Purpose:** The routine calculates an approximation result to a given Fourier integral I = Integral of F(X)*W(X) over (A,INFINITY) where W(X)=COS(OMEGA*X) or W(X)=SIN(OMEGA*X), hopefully satisfying following claim for accuracy ABS(I-RESULT).LE.EPSABS
**Source:** `dqawfe.f`

### DQAWO
**Purpose:** Calculate an approximation to a given definite integral I= Integral of F(X)*W(X) over (A,B), where W(X) = COS(OMEGA*X) or W(X) = SIN(OMEGA*X), hopefully satisfying the following claim for accuracy ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I))
**Source:** `dqawo.f`

### DQAWOE
**Purpose:** Calculate an approximation to a given definite integral I = Integral of F(X)*W(X) over (A,B), where W(X) = COS(OMEGA*X) or W(X)=SIN(OMEGA*X), hopefully satisfying the following claim for accuracy ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I))
**Source:** `dqawoe.f`

### DQAWS
**Purpose:** The routine calculates an approximation result to a given definite integral I = Integral of F*W over (A,B), (where W shows a singular behaviour at the end points see parameter INTEGR). Hopefully satisfying following claim for accuracy ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I))
**Source:** `dqaws.f`

### DQAWSE
**Purpose:** The routine calculates an approximation result to a given definite integral I = Integral of F*W over (A,B), (where W shows a singular behaviour at the end points, see parameter INTEGR). Hopefully satisfying following claim for accuracy ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I))
**Source:** `dqawse.f`

### DQC25C
**Purpose:** To compute I = Integral of F*W over (A,B) with error estimate, where W(X) = 1/(X-C)
**Source:** `dqc25c.f`

### DQC25F
**Purpose:** To compute the integral I=Integral of F(X) over (A,B) Where W(X) = COS(OMEGA*X) or W(X)=SIN(OMEGA*X) and to compute J = Integral of ABS(F) over (A,B). For small value of OMEGA or small intervals (A,B) the 15-point GAUSS-KRONRO Rule is used. Otherwise a generalized CLENSHAW-CURTIS method is used
**Source:** `dqc25f.f`

### DQC25S
**Purpose:** To compute I = Integral of F*W over (BL,BR), with error estimate, where the weight function W has a singular behaviour of ALGEBRAICO-LOGARITHMIC type at the points A and/or B. (BL,BR) is a part of (A,B)
**Source:** `dqc25s.f`

### DQCHEB
**Purpose:** This routine computes the CHEBYSHEV series expansion of degrees 12 and 24 of a function using A FAST FOURIER TRANSFORM METHOD F(X) = SUM(K=1,..,13) (CHEB12(K)*T(K-1,X)), F(X) = SUM(K=1,..,25) (CHEB24(K)*T(K-1,X)), Where T(K,X) is the CHEBYSHEV POLYNOMIAL OF DEGREE K
**Source:** `dqcheb.f`

### DQDOTA
**Purpose:** Compute the inner product of two vectors with extended precision accumulation and result
**Source:** `dqdota.f`

### DQDOTI
**Purpose:** Compute the inner product of two vectors with extended precision accumulation and result
**Source:** `dqdoti.f`

### DQELG
**Purpose:** The routine determines the limit of a given sequence of approximations, by means of the Epsilon algorithm of P.Wynn. An estimate of the absolute error is also given. The condensed Epsilon table is computed. Only those elements needed for the computation of the next diagonal are preserved
**Source:** `dqelg.f`

### DQFORM
**Purpose:** Subsidiary to DNSQ and DNSQE
**Source:** `dqform.f`

### DQK15
**Purpose:** To compute I = Integral of F over (A,B), with error estimate J = integral of ABS(F) over (A,B)
**Source:** `dqk15.f`

### DQK15I
**Purpose:** The original (infinite integration range is mapped onto the interval (0,1) and (A,B) is a part of (0,1). it is the purpose to compute I = Integral of transformed integrand over (A,B), J = Integral of ABS(Transformed Integrand) over (A,B)
**Source:** `dqk15i.f`

### DQK15W
**Purpose:** To compute I = Integral of F*W over (A,B), with error estimate J = Integral of ABS(F*W) over (A,B)
**Source:** `dqk15w.f`

### DQK21
**Purpose:** To compute I = Integral of F over (A,B), with error estimate J = Integral of ABS(F) over (A,B)
**Source:** `dqk21.f`

### DQK31
**Purpose:** To compute I = Integral of F over (A,B) with error estimate J = Integral of ABS(F) over (A,B)
**Source:** `dqk31.f`

### DQK41
**Purpose:** To compute I = Integral of F over (A,B), with error estimate J = Integral of ABS(F) over (A,B)
**Source:** `dqk41.f`

### DQK51
**Purpose:** To compute I = Integral of F over (A,B) with error estimate J = Integral of ABS(F) over (A,B)
**Source:** `dqk51.f`

### DQK61
**Purpose:** To compute I = Integral of F over (A,B) with error estimate J = Integral of ABS(F) over (A,B)
**Source:** `dqk61.f`

### DQMOMO
**Purpose:** This routine computes modified Chebyshev moments. The K-th modified Chebyshev moment is defined as the integral over (-1,1) of W(X)*T(K,X), where T(K,X) is the Chebyshev polynomial of degree K
**Source:** `dqmomo.f`

### DQNC79
**Purpose:** Integrate a function using a 7-point adaptive Newton-Cotes quadrature rule
**Source:** `dqnc79.f`

### DQNG
**Purpose:** The routine calculates an approximation result to a given definite integral I = integral of F over (A,B), hopefully satisfying following claim for accuracy ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I))
**Source:** `dqng.f`

### DQPSRT
**Purpose:** This routine maintains the descending ordering in the list of the local error estimated resulting from the interval subdivision process. At each call two error estimates are inserted using the sequential search method, top-down for the largest error estimate and bottom-up for the smallest error estimate
**Source:** `dqpsrt.f`

### DQRFAC
**Purpose:** Subsidiary to DNLS1, DNLS1E, DNSQ and DNSQE
**Source:** `dqrfac.f`

### DQRSLV
**Purpose:** Subsidiary to DNLS1 and DNLS1E
**Source:** `dqrslv.f`

### DQWGTC
**Purpose:** This function subprogram is used together with the routine DQAWC and defines the WEIGHT function
**Source:** `dqwgtc.f`

### DQWGTF
**Purpose:** This function subprogram is used together with the routine DQAWF and defines the WEIGHT function
**Source:** `dqwgtf.f`

### DQWGTS
**Purpose:** This function subprogram is used together with the routine DQAWS and defines the WEIGHT function
**Source:** `dqwgts.f`

### DRC
**Purpose:** Calculate a double precision approximation to DRC(X,Y) = Integral from zero to infinity of -1/2 -1 (1/2)(t+X) (t+Y) dt, where X is nonnegative and Y is positive
**Source:** `drc.f`

### DRC3JJ
**Purpose:** Evaluate the 3j symbol f(L1) = ( L1 L2 L3) (-M2-M3 M2 M3) for all allowed values of L1, the other parameters being held fixed
**Source:** `drc3jj.f`

### DRC3JM
**Purpose:** Evaluate the 3j symbol g(M2) = (L1 L2 L3 ) (M1 M2 -M1-M2) for all allowed values of M2, the other parameters being held fixed
**Source:** `drc3jm.f`

### DRC6J
**Purpose:** Evaluate the 6j symbol h(L1) = {L1 L2 L3} {L4 L5 L6} for all allowed values of L1, the other parameters being held fixed
**Source:** `drc6j.f`

### DRD
**Purpose:** Compute the incomplete or complete elliptic integral of the 2nd kind. For X and Y nonnegative, X+Y and Z positive, DRD(X,Y,Z) = Integral from zero to infinity of -1/2 -1/2 -3/2 (3/2)(t+X) (t+Y) (t+Z) dt. If X or Y is zero, the integral is complete
**Source:** `drd.f`

### DREADP
**Purpose:** Subsidiary to DSPLP
**Source:** `dreadp.f`

### DREORT
**Purpose:** Subsidiary to DBVSUP
**Source:** `dreort.f`

### DRF
**Purpose:** Compute the incomplete or complete elliptic integral of the 1st kind. For X, Y, and Z non-negative and at most one of them zero, RF(X,Y,Z) = Integral from zero to infinity of -1/2 -1/2 -1/2 (1/2)(t+X) (t+Y) (t+Z) dt. If X, Y or Z is zero, the integral is complete
**Source:** `drf.f`

### DRJ
**Purpose:** Compute the incomplete or complete (X or Y or Z is zero) elliptic integral of the 3rd kind. For X, Y, and Z non- negative, at most one of them zero, and P positive, RJ(X,Y,Z,P) = Integral from zero to infinity of -1/2 -1/2 -1/2 -1 (3/2)(t+X) (t+Y) (t+Z) (t+P) dt
**Source:** `drj.f`

### DRKFAB
**Purpose:** Subsidiary to DBVSUP
**Source:** `drkfab.f`

### DRKFS
**Purpose:** Subsidiary to DDERKF
**Source:** `drkfs.f`

### DRSCO
**Purpose:** Subsidiary to DDEBDF
**Source:** `drsco.f`

### DSLVS
**Purpose:** Subsidiary to DDEBDF
**Source:** `dslvs.f`

### DSORT
**Purpose:** Sort an array and optionally make the same interchanges in an auxiliary array. The array may be sorted in increasing or decreasing order. A slightly modified QUICKSORT algorithm is used
**Source:** `dsort.f`

### DSOS
**Purpose:** Solve a square system of nonlinear equations
**Source:** `dsos.f`

### DSOSEQ
**Purpose:** Subsidiary to DSOS
**Source:** `dsoseq.f`

### DSOSSL
**Purpose:** Subsidiary to DSOS
**Source:** `dsossl.f`

### DSPLP
**Purpose:** Solve linear programming problems involving at most a few thousand constraints and variables. Takes advantage of sparsity in the constraint matrix
**Source:** `dsplp.f`

### DSTEPS
**Purpose:** Integrate a system of first order ordinary differential equations one step
**Source:** `dsteps.f`

### DSTOD
**Purpose:** Subsidiary to DDEBDF
**Source:** `dstod.f`

### DSTOR1
**Purpose:** Subsidiary to DBVSUP
**Source:** `dstor1.f`

### DSTWAY
**Purpose:** Subsidiary to DBVSUP
**Source:** `dstway.f`

### DSUDS
**Purpose:** Subsidiary to DBVSUP
**Source:** `dsuds.f`

### DSVCO
**Purpose:** Subsidiary to DDEBDF
**Source:** `dsvco.f`

### DU11LS
**Purpose:** Subsidiary to DLLSIA
**Source:** `du11ls.f`

### DU11US
**Purpose:** Subsidiary to DULSIA
**Source:** `du11us.f`

### DU12LS
**Purpose:** Subsidiary to DLLSIA
**Source:** `du12ls.f`

### DU12US
**Purpose:** Subsidiary to DULSIA
**Source:** `du12us.f`

### DULSIA
**Purpose:** Solve an underdetermined linear system of equations by performing an LQ factorization of the matrix using Householder transformations. Emphasis is put on detecting possible rank deficiency
**Source:** `dulsia.f`

### DUSRMT
**Purpose:** Subsidiary to DSPLP
**Source:** `dusrmt.f`

### DVECS
**Purpose:** Subsidiary to DBVSUP
**Source:** `dvecs.f`

### DVNRMS
**Purpose:** Subsidiary to DDEBDF
**Source:** `dvnrms.f`

### DVOUT
**Purpose:** Subsidiary to DSPLP
**Source:** `dvout.f`

### DWNLIT
**Purpose:** Subsidiary to DWNNLS
**Source:** `dwnlit.f`

### DWNLSM
**Purpose:** Subsidiary to DWNNLS
**Source:** `dwnlsm.f`

### DWNLT1
**Purpose:** Subsidiary to WNLIT
**Source:** `dwnlt1.f`

### DWNLT2
**Purpose:** Subsidiary to WNLIT
**Source:** `dwnlt2.f`

### DWNLT3
**Purpose:** Subsidiary to WNLIT
**Source:** `dwnlt3.f`

### DWNNLS
**Purpose:** Solve a linearly constrained least squares problem with equality constraints and nonnegativity constraints on selected variables
**Source:** `dwnnls.f`

### DWRITP
**Purpose:** Subsidiary to DSPLP
**Source:** `dwritp.f`

### DWUPDT
**Purpose:** Subsidiary to DNLS1 and DNLS1E
**Source:** `dwupdt.f`

### DX
**Purpose:** Subsidiary to SEPELI
**Source:** `dx.f`

### DX4
**Purpose:** Subsidiary to SEPX4
**Source:** `dx4.f`

### DXADD
**Purpose:** To provide double-precision floating-point arithmetic with an extended exponent range
**Source:** `dxadd.f`

### DXADJ
**Purpose:** To provide double-precision floating-point arithmetic with an extended exponent range
**Source:** `dxadj.f`

### DXC210
**Purpose:** To provide double-precision floating-point arithmetic with an extended exponent range
**Source:** `dxc210.f`

### DXCON
**Purpose:** To provide double-precision floating-point arithmetic with an extended exponent range
**Source:** `dxcon.f`

### DXLEGF
**Purpose:** Compute normalized Legendre polynomials and associated Legendre functions
**Source:** `dxlegf.f`

### DXNRMP
**Purpose:** Compute normalized Legendre polynomials
**Source:** `dxnrmp.f`

### DXPMU
**Purpose:** To compute the values of Legendre functions for DXLEGF. Method: backward mu-wise recurrence for P(-MU,NU,X) for fixed nu to obtain P(-MU2,NU1,X), P(-(MU2-1),NU1,X), ..., P(-MU1,NU1,X) and store in ascending mu order
**Source:** `dxpmu.f`

### DXPMUP
**Purpose:** To compute the values of Legendre functions for DXLEGF. This subroutine transforms an array of Legendre functions of the first kind of negative order stored in array PQA into Legendre functions of the first kind of positive order stored in array PQA. The original array is destroyed
**Source:** `dxpmup.f`

### DXPNRM
**Purpose:** To compute the values of Legendre functions for DXLEGF. This subroutine transforms an array of Legendre functions of the first kind of negative order stored in array PQA into normalized Legendre polynomials stored in array PQA. The original array is destroyed
**Source:** `dxpnrm.f`

### DXPQNU
**Purpose:** To compute the values of Legendre functions for DXLEGF. This subroutine calculates initial values of P or Q using power series, then performs forward nu-wise recurrence to obtain P(-MU,NU,X), Q(0,NU,X), or Q(1,NU,X). The nu-wise recurrence is stable for P for all mu and for Q for mu=0,1
**Source:** `dxpqnu.f`

### DXPSI
**Purpose:** To compute values of the Psi function for DXLEGF
**Source:** `dxpsi.f`

### DXQMU
**Purpose:** To compute the values of Legendre functions for DXLEGF. Method: forward mu-wise recurrence for Q(MU,NU,X) for fixed nu to obtain Q(MU1,NU,X), Q(MU1+1,NU,X), ..., Q(MU2,NU,X)
**Source:** `dxqmu.f`

### DXQNU
**Purpose:** To compute the values of Legendre functions for DXLEGF. Method: backward nu-wise recurrence for Q(MU,NU,X) for fixed mu to obtain Q(MU1,NU1,X), Q(MU1,NU1+1,X), ..., Q(MU1,NU2,X)
**Source:** `dxqnu.f`

### DXRED
**Purpose:** To provide double-precision floating-point arithmetic with an extended exponent range
**Source:** `dxred.f`

### DXSET
**Purpose:** To provide double-precision floating-point arithmetic with an extended exponent range
**Source:** `dxset.f`

### DY
**Purpose:** Subsidiary to SEPELI
**Source:** `dy.f`

### DY4
**Purpose:** Subsidiary to SEPX4
**Source:** `dy4.f`

### DYAIRY
**Purpose:** Subsidiary to DBESJ and DBESY
**Source:** `dyairy.f`


## E

### EFC
**Purpose:** Fit a piecewise polynomial curve to discrete data. The piecewise polynomials are represented as B-splines. The fitting is done in a weighted least squares sense
**Source:** `efc.f`

### EFCMN
**Purpose:** Subsidiary to EFC
**Source:** `efcmn.f`

### ENORM
**Purpose:** Subsidiary to SNLS1, SNLS1E, SNSQ and SNSQE
**Source:** `enorm.f`

### EXBVP
**Purpose:** Subsidiary to BVSUP
**Source:** `exbvp.f`

### EXINT
**Purpose:** Compute an M member sequence of exponential integrals E(N+K,X), K=0,1,...,M-1 for N .GE. 1 and X .GE. 0
**Source:** `exint.f`


## F

### FC
**Purpose:** Fit a piecewise polynomial curve to discrete data. The piecewise polynomials are represented as B-splines. The fitting is done in a weighted least squares sense. Equality and inequality constraints can be imposed on the fitted curve
**Source:** `fc.f`

### FCMN
**Purpose:** Subsidiary to FC
**Source:** `fcmn.f`

### FDJAC1
**Purpose:** Subsidiary to SNSQ and SNSQE
**Source:** `fdjac1.f`

### FDJAC3
**Purpose:** Subsidiary to SNLS1 and SNLS1E
**Source:** `fdjac3.f`

### FDUMP
**Purpose:** Symbolic dump (should be locally written)
**Source:** `fdump.f`

### FFTDOC
**Purpose:** Documentation for FFTPACK, a collection of Fast Fourier Transform routines
**Source:** `fftdoc.f`

### FULMAT
**Purpose:** Subsidiary to SPLP
**Source:** `fulmat.f`

### FUNDOC
**Purpose:** Documentation for FNLIB, a collection of routines for evaluating elementary and special functions
**Source:** `fundoc.f`

### FZERO
**Purpose:** Search for a zero of a function F(X) in a given interval (B,C). It is designed primarily for problems where F(B) and F(C) have opposite signs
**Source:** `fzero.f`


## G

### GAMLN
**Purpose:** Compute the logarithm of the Gamma function
**Source:** `gamln.f`

### GAMRN
**Purpose:** Subsidiary to BSKIN
**Source:** `gamrn.f`

### GAUS8
**Purpose:** Integrate a real function of one variable over a finite interval using an adaptive 8-point Legendre-Gauss algorithm. Intended primarily for high accuracy integration or integration of smooth functions
**Source:** `gaus8.f`


## H

### H12
**Purpose:** Subsidiary to HFTI, LSEI and WNNLS
**Source:** `h12.f`

### HFTI
**Purpose:** Solve a linear least squares problems by performing a QR factorization of the matrix using Householder transformations
**Source:** `hfti.f`

### HKSEQ
**Purpose:** Subsidiary to BSKIN
**Source:** `hkseq.f`

### HPPERM
**Purpose:** Rearrange a given array according to a prescribed permutation vector
**Source:** `hpperm.f`

### HPSORT
**Purpose:** Return the permutation vector generated by sorting a substring within a character array and, optionally, rearrange the elements of the array. The array may be sorted in forward or reverse lexicographical order. A slightly modified quicksort algorithm is used
**Source:** `hpsort.f`

### HSTART
**Purpose:** Subsidiary to DEABM, DEBDF and DERKF
**Source:** `hstart.f`

### HSTCS1
**Purpose:** Subsidiary to HSTCSP
**Source:** `hstcs1.f`

### HVNRM
**Purpose:** Subsidiary to DEABM, DEBDF and DERKF
**Source:** `hvnrm.f`

### HWSCS1
**Purpose:** Subsidiary to HWSCSP
**Source:** `hwscs1.f`

### HWSSS1
**Purpose:** Subsidiary to HWSSSP
**Source:** `hwsss1.f`


## I

### I1MACH
**Purpose:** Return integer machine dependent constants
**Source:** `i1mach.f`

### I1MERG
**Purpose:** Merge two strings of ascending integers
**Source:** `i1merg.f`

### IDLOC
**Purpose:** Subsidiary to DSPLP
**Source:** `idloc.f`

### INDXA
**Purpose:** Subsidiary to BLKTRI
**Source:** `indxa.f`

### INDXB
**Purpose:** Subsidiary to BLKTRI
**Source:** `indxb.f`

### INDXC
**Purpose:** Subsidiary to BLKTRI
**Source:** `indxc.f`

### INTRV
**Purpose:** Compute the largest integer ILEFT in 1 .LE. ILEFT .LE. LXT such that XT(ILEFT) .LE. X where XT(*) is a subdivision of the X interval
**Source:** `intrv.f`

### INTYD
**Purpose:** Subsidiary to DEBDF
**Source:** `intyd.f`

### INXCA
**Purpose:** Subsidiary to CBLKTR
**Source:** `inxca.f`

### INXCB
**Purpose:** Subsidiary to CBLKTR
**Source:** `inxcb.f`

### INXCC
**Purpose:** Subsidiary to CBLKTR
**Source:** `inxcc.f`

### IPLOC
**Purpose:** Subsidiary to SPLP
**Source:** `iploc.f`

### IPPERM
**Purpose:** Rearrange a given array according to a prescribed permutation vector
**Source:** `ipperm.f`

### IPSORT
**Purpose:** Return the permutation vector generated by sorting a given array and, optionally, rearrange the elements of the array. The array may be sorted in increasing or decreasing order. A slightly modified quicksort algorithm is used
**Source:** `ipsort.f`

### ISORT
**Purpose:** Sort an array and optionally make the same interchanges in an auxiliary array. The array may be sorted in increasing or decreasing order. A slightly modified QUICKSORT algorithm is used
**Source:** `isort.f`

### IVOUT
**Purpose:** Subsidiary to SPLP
**Source:** `ivout.f`


## J

### J4SAVE
**Purpose:** Save or recall global variables needed by error handling routines
**Source:** `j4save.f`

### JAIRY
**Purpose:** Subsidiary to BESJ and BESY
**Source:** `jairy.f`


## L

### LA05AD
**Purpose:** Subsidiary to DSPLP
**Source:** `la05ad.f`

### LA05AS
**Purpose:** Subsidiary to SPLP
**Source:** `la05as.f`

### LA05BD
**Purpose:** Subsidiary to DSPLP
**Source:** `la05bd.f`

### LA05BS
**Purpose:** Subsidiary to SPLP
**Source:** `la05bs.f`

### LA05CD
**Purpose:** Subsidiary to DSPLP
**Source:** `la05cd.f`

### LA05CS
**Purpose:** Subsidiary to SPLP
**Source:** `la05cs.f`

### LA05ED
**Purpose:** Subsidiary to DSPLP
**Source:** `la05ed.f`

### LA05ES
**Purpose:** Subsidiary to SPLP
**Source:** `la05es.f`

### LAMW
**Purpose:** Compute the principal branch of the Lambert W function
**Source:** `lamw.f`

### LLSIA
**Purpose:** Solve a linear least squares problems by performing a QR factorization of the matrix using Householder transformations. Emphasis is put on detecting possible rank deficiency
**Source:** `llsia.f`

### LMPAR
**Purpose:** Subsidiary to SNLS1 and SNLS1E
**Source:** `lmpar.f`

### LPDP
**Purpose:** Subsidiary to LSEI
**Source:** `lpdp.f`

### LSAME
**Purpose:** Test two characters to determine if they are the same letter, except for case
**Source:** `lsame.f`

### LSEI
**Purpose:** Solve a linearly constrained least squares problem with equality and inequality constraints, and optionally compute a covariance matrix
**Source:** `lsei.f`

### LSI
**Purpose:** Subsidiary to LSEI
**Source:** `lsi.f`

### LSOD
**Purpose:** Subsidiary to DEBDF
**Source:** `lsod.f`

### LSSODS
**Purpose:** Subsidiary to BVSUP
**Source:** `lssods.f`

### LSSUDS
**Purpose:** Subsidiary to BVSUP
**Source:** `lssuds.f`


## M

### MACON
**Purpose:** Subsidiary to BVSUP
**Source:** `macon.f`

### MC20AD
**Purpose:** Subsidiary to DSPLP
**Source:** `mc20ad.f`

### MC20AS
**Purpose:** Subsidiary to SPLP
**Source:** `mc20as.f`

### MGSBV
**Purpose:** Subsidiary to BVSUP
**Source:** `mgsbv.f`

### MINSO4
**Purpose:** Subsidiary to SEPX4
**Source:** `minso4.f`

### MINSOL
**Purpose:** Subsidiary to SEPELI
**Source:** `minsol.f`

### MPADD
**Purpose:** Subsidiary to DQDOTA and DQDOTI
**Source:** `mpadd.f`

### MPADD2
**Purpose:** Subsidiary to DQDOTA and DQDOTI
**Source:** `mpadd2.f`

### MPADD3
**Purpose:** Subsidiary to DQDOTA and DQDOTI
**Source:** `mpadd3.f`

### MPBLAS
**Purpose:** Subsidiary to DQDOTA and DQDOTI
**Source:** `mpblas.f`

### MPCDM
**Purpose:** Subsidiary to DQDOTA and DQDOTI
**Source:** `mpcdm.f`

### MPCHK
**Purpose:** Subsidiary to DQDOTA and DQDOTI
**Source:** `mpchk.f`

### MPCMD
**Purpose:** Subsidiary to DQDOTA and DQDOTI
**Source:** `mpcmd.f`

### MPDIVI
**Purpose:** Subsidiary to DQDOTA and DQDOTI
**Source:** `mpdivi.f`

### MPERR
**Purpose:** Subsidiary to DQDOTA and DQDOTI
**Source:** `mperr.f`

### MPMAXR
**Purpose:** Subsidiary to DQDOTA and DQDOTI
**Source:** `mpmaxr.f`

### MPMLP
**Purpose:** Subsidiary to DQDOTA and DQDOTI
**Source:** `mpmlp.f`

### MPMUL
**Purpose:** Subsidiary to DQDOTA and DQDOTI
**Source:** `mpmul.f`

### MPMUL2
**Purpose:** Subsidiary to DQDOTA and DQDOTI
**Source:** `mpmul2.f`

### MPMULI
**Purpose:** Subsidiary to DQDOTA and DQDOTI
**Source:** `mpmuli.f`

### MPNZR
**Purpose:** Subsidiary to DQDOTA and DQDOTI
**Source:** `mpnzr.f`

### MPOVFL
**Purpose:** Subsidiary to DQDOTA and DQDOTI
**Source:** `mpovfl.f`

### MPSTR
**Purpose:** Subsidiary to DQDOTA and DQDOTI
**Source:** `mpstr.f`

### MPUNFL
**Purpose:** Subsidiary to DQDOTA and DQDOTI
**Source:** `mpunfl.f`


## N

### NUMXER
**Purpose:** Return the most recent error number
**Source:** `numxer.f`


## O

### OHTROL
**Purpose:** Subsidiary to BVSUP
**Source:** `ohtrol.f`

### OHTROR
**Purpose:** Subsidiary to BVSUP
**Source:** `ohtror.f`

### ORTHO4
**Purpose:** Subsidiary to SEPX4
**Source:** `ortho4.f`

### ORTHOG
**Purpose:** Subsidiary to SEPELI
**Source:** `orthog.f`

### ORTHOL
**Purpose:** Subsidiary to BVSUP
**Source:** `orthol.f`

### ORTHOR
**Purpose:** Subsidiary to BVSUP
**Source:** `orthor.f`


## P

### PCHNGS
**Purpose:** Subsidiary to SPLP
**Source:** `pchngs.f`

### PCOEF
**Purpose:** Convert the POLFIT coefficients to Taylor series form
**Source:** `pcoef.f`

### PFQAD
**Purpose:** Compute the integral on (X1,X2) of a product of a function F and the ID-th derivative of a B-spline, (PP-representation)
**Source:** `pfqad.f`

### PGSF
**Purpose:** Subsidiary to CBLKTR
**Source:** `pgsf.f`

### PIMACH
**Purpose:** Subsidiary to HSTCSP, HSTSSP and HWSCSP
**Source:** `pimach.f`

### PINITM
**Purpose:** Subsidiary to SPLP
**Source:** `pinitm.f`

### PJAC
**Purpose:** Subsidiary to DEBDF
**Source:** `pjac.f`

### PNNZRS
**Purpose:** Subsidiary to SPLP
**Source:** `pnnzrs.f`

### POISD2
**Purpose:** Subsidiary to GENBUN
**Source:** `poisd2.f`

### POISN2
**Purpose:** Subsidiary to GENBUN
**Source:** `poisn2.f`

### POISP2
**Purpose:** Subsidiary to GENBUN
**Source:** `poisp2.f`

### POLCOF
**Purpose:** Compute the coefficients of the polynomial fit (including Hermite polynomial fits) produced by a previous call to POLINT
**Source:** `polcof.f`

### POLFIT
**Purpose:** Fit discrete data in a least squares sense by polynomials in one variable
**Source:** `polfit.f`

### POLINT
**Purpose:** Produce the polynomial which interpolates a set of discrete data points
**Source:** `polint.f`

### POLYVL
**Purpose:** Calculate the value of a polynomial and its first NDER derivatives where the polynomial was produced by a previous call to POLINT
**Source:** `polyvl.f`

### POS3D1
**Purpose:** Subsidiary to POIS3D
**Source:** `pos3d1.f`

### POSTG2
**Purpose:** Subsidiary to POISTG
**Source:** `postg2.f`

### PPADD
**Purpose:** Subsidiary to BLKTRI
**Source:** `ppadd.f`

### PPGQ8
**Purpose:** Subsidiary to PFQAD
**Source:** `ppgq8.f`

### PPGSF
**Purpose:** Subsidiary to CBLKTR
**Source:** `ppgsf.f`

### PPPSF
**Purpose:** Subsidiary to CBLKTR
**Source:** `pppsf.f`

### PPQAD
**Purpose:** Compute the integral on (X1,X2) of a K-th order B-spline using the piecewise polynomial (PP) representation
**Source:** `ppqad.f`

### PPSGF
**Purpose:** Subsidiary to BLKTRI
**Source:** `ppsgf.f`

### PPSPF
**Purpose:** Subsidiary to BLKTRI
**Source:** `ppspf.f`

### PPVAL
**Purpose:** Calculate the value of the IDERIV-th derivative of the B-spline from the PP-representation
**Source:** `ppval.f`

### PROC
**Purpose:** Subsidiary to CBLKTR
**Source:** `proc.f`

### PROCP
**Purpose:** Subsidiary to CBLKTR
**Source:** `procp.f`

### PROD
**Purpose:** Subsidiary to BLKTRI
**Source:** `prod.f`

### PRODP
**Purpose:** Subsidiary to BLKTRI
**Source:** `prodp.f`

### PRVEC
**Purpose:** Subsidiary to BVSUP
**Source:** `prvec.f`

### PRWPGE
**Purpose:** Subsidiary to SPLP
**Source:** `prwpge.f`

### PRWVIR
**Purpose:** Subsidiary to SPLP
**Source:** `prwvir.f`

### PSGF
**Purpose:** Subsidiary to BLKTRI
**Source:** `psgf.f`

### PSIFN
**Purpose:** Compute derivatives of the Psi function
**Source:** `psifn.f`

### PSIXN
**Purpose:** Subsidiary to EXINT
**Source:** `psixn.f`

### PVALUE
**Purpose:** Use the coefficients generated by POLFIT to evaluate the polynomial fit of degree L, along with the first NDER of its derivatives, at a specified point
**Source:** `pvalue.f`

### PYTHAG
**Purpose:** Compute the complex square root of a complex number without destructive overflow or underflow
**Source:** `pythag.f`


## Q

### QAG
**Purpose:** The routine calculates an approximation result to a given definite integral I = integral of F over (A,B), hopefully satisfying following claim for accuracy ABS(I-RESULT)LE.MAX(EPSABS,EPSREL*ABS(I))
**Source:** `qag.f`

### QAGE
**Purpose:** The routine calculates an approximation result to a given definite integral I = Integral of F over (A,B), hopefully satisfying following claim for accuracy ABS(I-RESLT).LE.MAX(EPSABS,EPSREL*ABS(I))
**Source:** `qage.f`

### QAGI
**Purpose:** The routine calculates an approximation result to a given INTEGRAL I = Integral of F over (BOUND,+INFINITY) OR I = Integral of F over (-INFINITY,BOUND) OR I = Integral of F over (-INFINITY,+INFINITY) Hopefully satisfying following claim for accuracy ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I))
**Source:** `qagi.f`

### QAGIE
**Purpose:** The routine calculates an approximation result to a given integral I = Integral of F over (BOUND,+INFINITY) or I = Integral of F over (-INFINITY,BOUND) or I = Integral of F over (-INFINITY,+INFINITY), hopefully satisfying following claim for accuracy ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I))
**Source:** `qagie.f`

### QAGP
**Purpose:** The routine calculates an approximation result to a given definite integral I = Integral of F over (A,B), hopefully satisfying following claim for accuracy break points of the integration interval, where local difficulties of the integrand may occur(e.g. SINGULARITIES, DISCONTINUITIES), are provided by the user
**Source:** `qagp.f`

### QAGPE
**Purpose:** Approximate a given definite integral I = Integral of F over (A,B), hopefully satisfying the accuracy claim: ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I)). Break points of the integration interval, where local difficulties of the integrand may occur (e.g. singularities or discontinuities) are provided by the user
**Source:** `qagpe.f`

### QAGS
**Purpose:** The routine calculates an approximation result to a given Definite integral I = Integral of F over (A,B), Hopefully satisfying following claim for accuracy ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I))
**Source:** `qags.f`

### QAGSE
**Purpose:** The routine calculates an approximation result to a given definite integral I = Integral of F over (A,B), hopefully satisfying following claim for accuracy ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I))
**Source:** `qagse.f`

### QAWC
**Purpose:** The routine calculates an approximation result to a Cauchy principal value I = INTEGRAL of F*W over (A,B) (W(X) = 1/((X-C), C.NE.A, C.NE.B), hopefully satisfying following claim for accuracy ABS(I-RESULT).LE.MAX(EPSABE,EPSREL*ABS(I))
**Source:** `qawc.f`

### QAWCE
**Purpose:** The routine calculates an approximation result to a CAUCHY PRINCIPAL VALUE I = Integral of F*W over (A,B) (W(X) = 1/(X-C), (C.NE.A, C.NE.B), hopefully satisfying following claim for accuracy ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I))
**Source:** `qawce.f`

### QAWF
**Purpose:** The routine calculates an approximation result to a given Fourier integral I = Integral of F(X)*W(X) over (A,INFINITY) where W(X) = COS(OMEGA*X) or W(X) = SIN(OMEGA*X). Hopefully satisfying following claim for accuracy ABS(I-RESULT).LE.EPSABS
**Source:** `qawf.f`

### QAWFE
**Purpose:** The routine calculates an approximation result to a given Fourier integral I = Integral of F(X)*W(X) over (A,INFINITY) where W(X) = COS(OMEGA*X) or W(X) = SIN(OMEGA*X), hopefully satisfying following claim for accuracy ABS(I-RESULT).LE.EPSABS
**Source:** `qawfe.f`

### QAWO
**Purpose:** Calculate an approximation to a given definite integral I = Integral of F(X)*W(X) over (A,B), where W(X) = COS(OMEGA*X) or W(X) = SIN(OMEGA*X), hopefully satisfying the following claim for accuracy ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I))
**Source:** `qawo.f`

### QAWOE
**Purpose:** Calculate an approximation to a given definite integral I = Integral of F(X)*W(X) over (A,B), where W(X) = COS(OMEGA*X) or W(X) = SIN(OMEGA*X), hopefully satisfying the following claim for accuracy ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I))
**Source:** `qawoe.f`

### QAWS
**Purpose:** The routine calculates an approximation result to a given definite integral I = Integral of F*W over (A,B), (where W shows a singular behaviour at the end points see parameter INTEGR). Hopefully satisfying following claim for accuracy ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I))
**Source:** `qaws.f`

### QAWSE
**Purpose:** The routine calculates an approximation result to a given definite integral I = Integral of F*W over (A,B), (where W shows a singular behaviour at the end points, see parameter INTEGR). Hopefully satisfying following claim for accuracy ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I))
**Source:** `qawse.f`

### QC25C
**Purpose:** To compute I = Integral of F*W over (A,B) with error estimate, where W(X) = 1/(X-C)
**Source:** `qc25c.f`

### QC25F
**Purpose:** To compute the integral I=Integral of F(X) over (A,B) Where W(X) = COS(OMEGA*X) Or (WX)=SIN(OMEGA*X) and to compute J=Integral of ABS(F) over (A,B). For small value of OMEGA or small intervals (A,B) 15-point GAUSS- KRONROD Rule used. Otherwise generalized CLENSHAW-CURTIS us
**Source:** `qc25f.f`

### QC25S
**Purpose:** To compute I = Integral of F*W over (BL,BR), with error estimate, where the weight function W has a singular behaviour of ALGEBRAICO-LOGARITHMIC type at the points A and/or B. (BL,BR) is a part of (A,B)
**Source:** `qc25s.f`

### QCHEB
**Purpose:** This routine computes the CHEBYSHEV series expansion of degrees 12 and 24 of a function using A FAST FOURIER TRANSFORM METHOD F(X) = SUM(K=1,..,13) (CHEB12(K)*T(K-1,X)), F(X) = SUM(K=1,..,25) (CHEB24(K)*T(K-1,X)), Where T(K,X) is the CHEBYSHEV POLYNOMIAL OF DEGREE K
**Source:** `qcheb.f`

### QELG
**Purpose:** The routine determines the limit of a given sequence of approximations, by means of the Epsilon algorithm of P. Wynn. An estimate of the absolute error is also given. The condensed Epsilon table is computed. Only those elements needed for the computation of the next diagonal are preserved
**Source:** `qelg.f`

### QFORM
**Purpose:** Subsidiary to SNSQ and SNSQE
**Source:** `qform.f`

### QK15
**Purpose:** To compute I = Integral of F over (A,B), with error estimate J = integral of ABS(F) over (A,B)
**Source:** `qk15.f`

### QK15I
**Purpose:** The original (infinite integration range is mapped onto the interval (0,1) and (A,B) is a part of (0,1). it is the purpose to compute I = Integral of transformed integrand over (A,B), J = Integral of ABS(Transformed Integrand) over (A,B)
**Source:** `qk15i.f`

### QK15W
**Purpose:** To compute I = Integral of F*W over (A,B), with error estimate J = Integral of ABS(F*W) over (A,B)
**Source:** `qk15w.f`

### QK21
**Purpose:** To compute I = Integral of F over (A,B), with error estimate J = Integral of ABS(F) over (A,B)
**Source:** `qk21.f`

### QK31
**Purpose:** To compute I = Integral of F over (A,B) with error estimate J = Integral of ABS(F) over (A,B)
**Source:** `qk31.f`

### QK41
**Purpose:** To compute I = Integral of F over (A,B), with error estimate J = Integral of ABS(F) over (A,B)
**Source:** `qk41.f`

### QK51
**Purpose:** To compute I = Integral of F over (A,B) with error estimate J = Integral of ABS(F) over (A,B)
**Source:** `qk51.f`

### QK61
**Purpose:** To compute I = Integral of F over (A,B) with error estimate J = Integral of ABS(F) over (A,B)
**Source:** `qk61.f`

### QMOMO
**Purpose:** This routine computes modified Chebyshev moments. The K-th modified Chebyshev moment is defined as the integral over (-1,1) of W(X)*T(K,X), where T(K,X) is the Chebyshev polynomial of degree K
**Source:** `qmomo.f`

### QNC79
**Purpose:** Integrate a function using a 7-point adaptive Newton-Cotes quadrature rule
**Source:** `qnc79.f`

### QNG
**Purpose:** The routine calculates an approximation result to a given definite integral I = integral of F over (A,B), hopefully satisfying following claim for accuracy ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I))
**Source:** `qng.f`

### QPDOC
**Purpose:** Documentation for QUADPACK, a package of subprograms for automatic evaluation of one-dimensional definite integrals
**Source:** `qpdoc.f`

### QPSRT
**Purpose:** Subsidiary to QAGE, QAGIE, QAGPE, QAGSE, QAWCE, QAWOE and QAWSE
**Source:** `qpsrt.f`

### QRFAC
**Purpose:** Subsidiary to SNLS1, SNLS1E, SNSQ and SNSQE
**Source:** `qrfac.f`

### QRSOLV
**Purpose:** Subsidiary to SNLS1 and SNLS1E
**Source:** `qrsolv.f`

### QWGTC
**Purpose:** This function subprogram is used together with the routine QAWC and defines the WEIGHT function
**Source:** `qwgtc.f`

### QWGTF
**Purpose:** This function subprogram is used together with the routine QAWF and defines the WEIGHT function
**Source:** `qwgtf.f`

### QWGTS
**Purpose:** This function subprogram is used together with the routine QAWS and defines the WEIGHT function
**Source:** `qwgts.f`


## R

### R1MACH
**Purpose:** Return floating point machine dependent constants
**Source:** `r1mach_ieee.f`

### R1MPYQ
**Purpose:** Subsidiary to SNSQ and SNSQE
**Source:** `r1mpyq.f`

### R1UPDT
**Purpose:** Subsidiary to SNSQ and SNSQE
**Source:** `r1updt.f`

### RC
**Purpose:** Calculate an approximation to RC(X,Y) = Integral from zero to infinity of -1/2 -1 (1/2)(t+X) (t+Y) dt, where X is nonnegative and Y is positive
**Source:** `rc.f`

### RC3JJ
**Purpose:** Evaluate the 3j symbol f(L1) = ( L1 L2 L3) (-M2-M3 M2 M3) for all allowed values of L1, the other parameters being held fixed
**Source:** `rc3jj.f`

### RC3JM
**Purpose:** Evaluate the 3j symbol g(M2) = (L1 L2 L3 ) (M1 M2 -M1-M2) for all allowed values of M2, the other parameters being held fixed
**Source:** `rc3jm.f`

### RC6J
**Purpose:** Evaluate the 6j symbol h(L1) = {L1 L2 L3} {L4 L5 L6} for all allowed values of L1, the other parameters being held fixed
**Source:** `rc6j.f`

### RD
**Purpose:** Compute the incomplete or complete elliptic integral of the 2nd kind. For X and Y nonnegative, X+Y and Z positive, RD(X,Y,Z) = Integral from zero to infinity of -1/2 -1/2 -3/2 (3/2)(t+X) (t+Y) (t+Z) dt. If X or Y is zero, the integral is complete
**Source:** `rd.f`

### REORT
**Purpose:** Subsidiary to BVSUP
**Source:** `reort.f`

### RF
**Purpose:** Compute the incomplete or complete elliptic integral of the 1st kind. For X, Y, and Z non-negative and at most one of them zero, RF(X,Y,Z) = Integral from zero to infinity of -1/2 -1/2 -1/2 (1/2)(t+X) (t+Y) (t+Z) dt. If X, Y or Z is zero, the integral is complete
**Source:** `rf.f`

### RJ
**Purpose:** Compute the incomplete or complete (X or Y or Z is zero) elliptic integral of the 3rd kind. For X, Y, and Z non- negative, at most one of them zero, and P positive, RJ(X,Y,Z,P) = Integral from zero to infinity of -1/2 -1/2 -1/2 -1 (3/2)(t+X) (t+Y) (t+Z) (t+P) dt
**Source:** `rj.f`

### RKFAB
**Purpose:** Subsidiary to BVSUP
**Source:** `rkfab.f`

### RPQR79
**Purpose:** Find the zeros of a polynomial with real coefficients
**Source:** `rpqr79.f`

### RPZERO
**Purpose:** Find the zeros of a polynomial with real coefficients
**Source:** `rpzero.f`

### RSCO
**Purpose:** Subsidiary to DEBDF
**Source:** `rsco.f`

### RWUPDT
**Purpose:** Subsidiary to SNLS1 and SNLS1E
**Source:** `rwupdt.f`


## S

### S1MERG
**Purpose:** Merge two strings of ascending real numbers
**Source:** `s1merg.f`

### SBOCLS
**Purpose:** Solve the bounded and constrained least squares problem consisting of solving the equation E*X = F (in the least squares sense) subject to the linear constraints C*X = Y
**Source:** `sbocls.f`

### SBOLS
**Purpose:** Solve the problem E*X = F (in the least squares sense) with bounds on selected X values
**Source:** `sbols.f`

### SBOLSM
**Purpose:** Subsidiary to SBOCLS and SBOLS
**Source:** `sbolsm.f`

### SCLOSM
**Purpose:** Subsidiary to SPLP
**Source:** `sclosm.f`

### SCOEF
**Purpose:** Subsidiary to BVSUP
**Source:** `scoef.f`

### SCOV
**Purpose:** Calculate the covariance matrix for a nonlinear data fitting problem. It is intended to be used after a successful return from either SNLS1 or SNLS1E
**Source:** `scov.f`

### SDAINI
**Purpose:** Initialization routine for SDASSL
**Source:** `sdaini.f`

### SDAJAC
**Purpose:** Compute the iteration matrix for SDASSL and form the LU-decomposition
**Source:** `sdajac.f`

### SDANRM
**Purpose:** Compute vector norm for SDASSL
**Source:** `sdanrm.f`

### SDASLV
**Purpose:** Linear system solver for SDASSL
**Source:** `sdaslv.f`

### SDASSL
**Purpose:** This code solves a system of differential/algebraic equations of the form G(T,Y,YPRIME) = 0
**Source:** `sdassl.f`

### SDASTP
**Purpose:** Perform one step of the SDASSL integration
**Source:** `sdastp.f`

### SDATRP
**Purpose:** Interpolation routine for SDASSL
**Source:** `sdatrp.f`

### SDAWTS
**Purpose:** Set error weight vector for SDASSL
**Source:** `sdawts.f`

### SDCOR
**Purpose:** Subroutine SDCOR computes corrections to the Y array
**Source:** `sdcor.f`

### SDCST
**Purpose:** SDCST sets coefficients used by the core integrator SDSTP
**Source:** `sdcst.f`

### SDNTL
**Purpose:** Subroutine SDNTL is called to set parameters on the first call to SDSTP, on an internal restart, or when the user has altered MINT, MITER, and/or H
**Source:** `sdntl.f`

### SDNTP
**Purpose:** Subroutine SDNTP interpolates the K-th derivative of Y at TOUT, using the data in the YH array. If K has a value greater than NQ, the NQ-th derivative is calculated
**Source:** `sdntp.f`

### SDPSC
**Purpose:** Subroutine SDPSC computes the predicted YH values by effectively multiplying the YH array by the Pascal triangle matrix when KSGN is +1, and performs the inverse function when KSGN is -1
**Source:** `sdpsc.f`

### SDPST
**Purpose:** Subroutine SDPST evaluates the Jacobian matrix of the right hand side of the differential equations
**Source:** `sdpst.f`

### SDRIV1
**Purpose:** The function of SDRIV1 is to solve N (200 or fewer) ordinary differential equations of the form dY(I)/dT = F(Y(I),T), given the initial conditions Y(I) = YI. SDRIV1 uses single precision arithmetic
**Source:** `sdriv1.f`

### SDRIV2
**Purpose:** The function of SDRIV2 is to solve N ordinary differential equations of the form dY(I)/dT = F(Y(I),T), given the initial conditions Y(I) = YI. The program has options to allow the solution of both stiff and non-stiff differential equations. SDRIV2 uses single precision arithmetic
**Source:** `sdriv2.f`

### SDRIV3
**Purpose:** The function of SDRIV3 is to solve N ordinary differential equations of the form dY(I)/dT = F(Y(I),T), given the initial conditions Y(I) = YI. The program has options to allow the solution of both stiff and non-stiff differential equations. Other important options are available. SDRIV3 uses single precision arithmetic
**Source:** `sdriv3.f`

### SDSCL
**Purpose:** Subroutine SDSCL rescales the YH array whenever the step size is changed
**Source:** `sdscl.f`

### SDSTP
**Purpose:** SDSTP performs one step of the integration of an initial value problem for a system of ordinary differential equations
**Source:** `sdstp.f`

### SDZRO
**Purpose:** SDZRO searches for a zero of a function F(N, T, Y, IROOT) between the given values B and C until the width of the interval (B, C) has collapsed to within a tolerance specified by the stopping criterion, ABS(B - C) .LE. 2.*(RW*ABS(B) + AE)
**Source:** `sdzro.f`

### SGEEV
**Purpose:** Compute the eigenvalues and, optionally, the eigenvectors of a real general matrix
**Source:** `sgeev.f`

### SGEFS
**Purpose:** Solve a general system of linear equations
**Source:** `sgefs.f`

### SGEIR
**Purpose:** Solve a general system of linear equations. Iterative refinement is used to obtain an error estimate
**Source:** `sgeir.f`

### SGLSS
**Purpose:** Solve a linear least squares problems by performing a QR factorization of the matrix using Householder transformations. Emphasis is put on detecting possible rank deficiency
**Source:** `sglss.f`

### SINTRP
**Purpose:** Approximate the solution at XOUT by evaluating the polynomial computed in STEPS at XOUT. Must be used in conjunction with STEPS
**Source:** `sintrp.f`

### SLVS
**Purpose:** Subsidiary to DEBDF
**Source:** `slvs.f`

### SMOUT
**Purpose:** Subsidiary to FC and SBOCLS
**Source:** `smout.f`

### SNBCO
**Purpose:** Factor a band matrix using Gaussian elimination and estimate the condition number
**Source:** `snbco.f`

### SNBDI
**Purpose:** Compute the determinant of a band matrix using the factors computed by SNBCO or SNBFA
**Source:** `snbdi.f`

### SNBFA
**Purpose:** Factor a real band matrix by elimination
**Source:** `snbfa.f`

### SNBFS
**Purpose:** Solve a general nonsymmetric banded system of linear equations
**Source:** `snbfs.f`

### SNBIR
**Purpose:** Solve a general nonsymmetric banded system of linear equations. Iterative refinement is used to obtain an error estimate
**Source:** `snbir.f`

### SNBSL
**Purpose:** Solve a real band system using the factors computed by SNBCO or SNBFA
**Source:** `snbsl.f`

### SNLS1
**Purpose:** Minimize the sum of the squares of M nonlinear functions in N variables by a modification of the Levenberg-Marquardt algorithm
**Source:** `snls1.f`

### SNLS1E
**Purpose:** An easy-to-use code which minimizes the sum of the squares of M nonlinear functions in N variables by a modification of the Levenberg-Marquardt algorithm
**Source:** `snls1e.f`

### SNSQ
**Purpose:** Find a zero of a system of a N nonlinear functions in N variables by a modification of the Powell hybrid method
**Source:** `snsq.f`

### SNSQE
**Purpose:** An easy-to-use code to find a zero of a system of N nonlinear functions in N variables by a modification of the Powell hybrid method
**Source:** `snsqe.f`

### SODS
**Purpose:** Subsidiary to BVSUP
**Source:** `sods.f`

### SOPENM
**Purpose:** Subsidiary to SPLP
**Source:** `sopenm.f`

### SOS
**Purpose:** Solve a square system of nonlinear equations
**Source:** `sos.f`

### SOSEQS
**Purpose:** Subsidiary to SOS
**Source:** `soseqs.f`

### SOSSOL
**Purpose:** Subsidiary to SOS
**Source:** `sossol.f`

### SPELI4
**Purpose:** Subsidiary to SEPX4
**Source:** `speli4.f`

### SPELIP
**Purpose:** Subsidiary to SEPELI
**Source:** `spelip.f`

### SPINCW
**Purpose:** Subsidiary to SPLP
**Source:** `spincw.f`

### SPINIT
**Purpose:** Subsidiary to SPLP
**Source:** `spinit.f`

### SPLP
**Purpose:** Solve linear programming problems involving at most a few thousand constraints and variables. Takes advantage of sparsity in the constraint matrix
**Source:** `splp.f`

### SPLPCE
**Purpose:** Subsidiary to SPLP
**Source:** `splpce.f`

### SPLPDM
**Purpose:** Subsidiary to SPLP
**Source:** `splpdm.f`

### SPLPFE
**Purpose:** Subsidiary to SPLP
**Source:** `splpfe.f`

### SPLPFL
**Purpose:** Subsidiary to SPLP
**Source:** `splpfl.f`

### SPLPMN
**Purpose:** Subsidiary to SPLP
**Source:** `splpmn.f`

### SPLPMU
**Purpose:** Subsidiary to SPLP
**Source:** `splpmu.f`

### SPLPUP
**Purpose:** Subsidiary to SPLP
**Source:** `splpup.f`

### SPOFS
**Purpose:** Solve a positive definite symmetric system of linear equations
**Source:** `spofs.f`

### SPOIR
**Purpose:** Solve a positive definite symmetric system of linear equations. Iterative refinement is used to obtain an error estimate
**Source:** `spoir.f`

### SPOPT
**Purpose:** Subsidiary to SPLP
**Source:** `spopt.f`

### SPPERM
**Purpose:** Rearrange a given array according to a prescribed permutation vector
**Source:** `spperm.f`

### SPSORT
**Purpose:** Return the permutation vector generated by sorting a given array and, optionally, rearrange the elements of the array. The array may be sorted in increasing or decreasing order. A slightly modified quicksort algorithm is used
**Source:** `spsort.f`

### SREADP
**Purpose:** Subsidiary to SPLP
**Source:** `sreadp.f`

### SSIEV
**Purpose:** Compute the eigenvalues and, optionally, the eigenvectors of a real symmetric matrix
**Source:** `ssiev.f`

### SSORT
**Purpose:** Sort an array and optionally make the same interchanges in an auxiliary array. The array may be sorted in increasing or decreasing order. A slightly modified QUICKSORT algorithm is used
**Source:** `ssort.f`

### STEPS
**Purpose:** Integrate a system of first order ordinary differential equations one step
**Source:** `steps.f`

### STOD
**Purpose:** Subsidiary to DEBDF
**Source:** `stod.f`

### STOR1
**Purpose:** Subsidiary to BVSUP
**Source:** `stor1.f`

### STWAY
**Purpose:** Subsidiary to BVSUP
**Source:** `stway.f`

### SUDS
**Purpose:** Subsidiary to BVSUP
**Source:** `suds.f`

### SVCO
**Purpose:** Subsidiary to DEBDF
**Source:** `svco.f`

### SVD
**Purpose:** Perform the singular value decomposition of a rectangular matrix
**Source:** `svd.f`

### SVECS
**Purpose:** Subsidiary to BVSUP
**Source:** `svecs.f`

### SVOUT
**Purpose:** Subsidiary to SPLP
**Source:** `svout.f`

### SWRITP
**Purpose:** Subsidiary to SPLP
**Source:** `swritp.f`


## T

### TEVLC
**Purpose:** Subsidiary to CBLKTR
**Source:** `tevlc.f`

### TEVLS
**Purpose:** Subsidiary to BLKTRI
**Source:** `tevls.f`

### TRI3
**Purpose:** Subsidiary to GENBUN
**Source:** `tri3.f`

### TRIDQ
**Purpose:** Subsidiary to POIS3D
**Source:** `tridq.f`

### TRIS4
**Purpose:** Subsidiary to SEPX4
**Source:** `tris4.f`

### TRISP
**Purpose:** Subsidiary to SEPELI
**Source:** `trisp.f`

### TRIX
**Purpose:** Subsidiary to GENBUN
**Source:** `trix.f`


## U

### U11LS
**Purpose:** Subsidiary to LLSIA
**Source:** `u11ls.f`

### U11US
**Purpose:** Subsidiary to ULSIA
**Source:** `u11us.f`

### U12LS
**Purpose:** Subsidiary to LLSIA
**Source:** `u12ls.f`

### U12US
**Purpose:** Subsidiary to ULSIA
**Source:** `u12us.f`

### ULSIA
**Purpose:** Solve an underdetermined linear system of equations by performing an LQ factorization of the matrix using Householder transformations. Emphasis is put on detecting possible rank deficiency
**Source:** `ulsia.f`

### USRMAT
**Purpose:** Subsidiary to SPLP
**Source:** `usrmat.f`


## V

### VNWRMS
**Purpose:** Subsidiary to DEBDF
**Source:** `vnwrms.f`


## W

### WNLIT
**Purpose:** Subsidiary to WNNLS
**Source:** `wnlit.f`

### WNLSM
**Purpose:** Subsidiary to WNNLS
**Source:** `wnlsm.f`

### WNLT1
**Purpose:** Subsidiary to WNLIT
**Source:** `wnlt1.f`

### WNLT2
**Purpose:** Subsidiary to WNLIT
**Source:** `wnlt2.f`

### WNLT3
**Purpose:** Subsidiary to WNLIT
**Source:** `wnlt3.f`

### WNNLS
**Purpose:** Solve a linearly constrained least squares problem with equality constraints and nonnegativity constraints on selected variables
**Source:** `wnnls.f`


## X

### XADD
**Purpose:** To provide single-precision floating-point arithmetic with an extended exponent range
**Source:** `xadd.f`

### XADJ
**Purpose:** To provide single-precision floating-point arithmetic with an extended exponent range
**Source:** `xadj.f`

### XC210
**Purpose:** To provide single-precision floating-point arithmetic with an extended exponent range
**Source:** `xc210.f`

### XCON
**Purpose:** To provide single-precision floating-point arithmetic with an extended exponent range
**Source:** `xcon.f`

### XERBLA
**Purpose:** Error handler for the Level 2 and Level 3 BLAS Routines
**Source:** `xerbla.f`

### XERCLR
**Purpose:** Reset current error number to zero
**Source:** `xerclr.f`

### XERCNT
**Purpose:** Allow user control over handling of errors
**Source:** `xercnt.f`

### XERDMP
**Purpose:** Print the error tables and then clear them
**Source:** `xerdmp.f`

### XERHLT
**Purpose:** Abort program execution and print error message
**Source:** `xerhlt.f`

### XERMAX
**Purpose:** Set maximum number of times any error message is to be printed
**Source:** `xermax.f`

### XERMSG
**Purpose:** Process error messages for SLATEC and other libraries
**Source:** `xermsg.f`

### XERPRN
**Purpose:** Print error messages processed by XERMSG
**Source:** `xerprn.f`

### XERSVE
**Purpose:** Record that an error has occurred
**Source:** `xersve.f`

### XGETF
**Purpose:** Return the current value of the error control flag
**Source:** `xgetf.f`

### XGETUA
**Purpose:** Return unit number(s) to which error messages are being sent
**Source:** `xgetua.f`

### XGETUN
**Purpose:** Return the (first) output file to which error messages are being sent
**Source:** `xgetun.f`

### XLEGF
**Purpose:** Compute normalized Legendre polynomials and associated Legendre functions
**Source:** `xlegf.f`

### XNRMP
**Purpose:** Compute normalized Legendre polynomials
**Source:** `xnrmp.f`

### XPMU
**Purpose:** To compute the values of Legendre functions for XLEGF. Method: backward mu-wise recurrence for P(-MU,NU,X) for fixed nu to obtain P(-MU2,NU1,X), P(-(MU2-1),NU1,X), ..., P(-MU1,NU1,X) and store in ascending mu order
**Source:** `xpmu.f`

### XPMUP
**Purpose:** To compute the values of Legendre functions for XLEGF. This subroutine transforms an array of Legendre functions of the first kind of negative order stored in array PQA into Legendre functions of the first kind of positive order stored in array PQA. The original array is destroyed
**Source:** `xpmup.f`

### XPNRM
**Purpose:** To compute the values of Legendre functions for XLEGF. This subroutine transforms an array of Legendre functions of the first kind of negative order stored in array PQA into normalized Legendre polynomials stored in array PQA. The original array is destroyed
**Source:** `xpnrm.f`

### XPQNU
**Purpose:** To compute the values of Legendre functions for XLEGF. This subroutine calculates initial values of P or Q using power series, then performs forward nu-wise recurrence to obtain P(-MU,NU,X), Q(0,NU,X), or Q(1,NU,X). The nu-wise recurrence is stable for P for all mu and for Q for mu=0,1
**Source:** `xpqnu.f`

### XPSI
**Purpose:** To compute values of the Psi function for XLEGF
**Source:** `xpsi.f`

### XQMU
**Purpose:** To compute the values of Legendre functions for XLEGF. Method: forward mu-wise recurrence for Q(MU,NU,X) for fixed nu to obtain Q(MU1,NU,X), Q(MU1+1,NU,X), ..., Q(MU2,NU,X)
**Source:** `xqmu.f`

### XQNU
**Purpose:** To compute the values of Legendre functions for XLEGF. Method: backward nu-wise recurrence for Q(MU,NU,X) for fixed mu to obtain Q(MU1,NU1,X), Q(MU1,NU1+1,X), ..., Q(MU1,NU2,X)
**Source:** `xqnu.f`

### XRED
**Purpose:** To provide single-precision floating-point arithmetic with an extended exponent range
**Source:** `xred.f`

### XSET
**Purpose:** To provide single-precision floating-point arithmetic with an extended exponent range
**Source:** `xset.f`

### XSETF
**Purpose:** Set the error control flag
**Source:** `xsetf.f`

### XSETUA
**Purpose:** Set logical unit numbers (up to 5) to which error messages are to be sent
**Source:** `xsetua.f`

### XSETUN
**Purpose:** Set output file to which error messages are to be sent
**Source:** `xsetun.f`


## Y

### YAIRY
**Purpose:** Subsidiary to BESJ and BESY
**Source:** `yairy.f`


## Z

### ZABS
**Purpose:** Subsidiary to ZBESH, ZBESI, ZBESJ, ZBESK, ZBESY, ZAIRY and ZBIRY
**Source:** `zabs.f`

### ZACAI
**Purpose:** Subsidiary to ZAIRY
**Source:** `zacai.f`

### ZACON
**Purpose:** Subsidiary to ZBESH and ZBESK
**Source:** `zacon.f`

### ZAIRY
**Purpose:** Compute the Airy function Ai(z) or its derivative dAi/dz for complex argument z. A scaling option is available to help avoid underflow and overflow
**Source:** `zairy.f`

### ZASYI
**Purpose:** Subsidiary to ZBESI and ZBESK
**Source:** `zasyi.f`

### ZBESH
**Purpose:** Compute a sequence of the Hankel functions H(m,a,z) for superscript m=1 or 2, real nonnegative orders a=b, b+1,... where b>0, and nonzero complex argument z. A scaling option is available to help avoid overflow
**Source:** `zbesh.f`

### ZBESI
**Purpose:** Compute a sequence of the Bessel functions I(a,z) for complex argument z and real nonnegative orders a=b,b+1, b+2,... where b>0. A scaling option is available to help avoid overflow
**Source:** `zbesi.f`

### ZBESJ
**Purpose:** Compute a sequence of the Bessel functions J(a,z) for complex argument z and real nonnegative orders a=b,b+1, b+2,... where b>0. A scaling option is available to help avoid overflow
**Source:** `zbesj.f`

### ZBESK
**Purpose:** Compute a sequence of the Bessel functions K(a,z) for complex argument z and real nonnegative orders a=b,b+1, b+2,... where b>0. A scaling option is available to help avoid overflow
**Source:** `zbesk.f`

### ZBESY
**Purpose:** Compute a sequence of the Bessel functions Y(a,z) for complex argument z and real nonnegative orders a=b,b+1, b+2,... where b>0. A scaling option is available to help avoid overflow
**Source:** `zbesy.f`

### ZBINU
**Purpose:** Subsidiary to ZAIRY, ZBESH, ZBESI, ZBESJ, ZBESK and ZBIRY
**Source:** `zbinu.f`

### ZBIRY
**Purpose:** Compute the Airy function Bi(z) or its derivative dBi/dz for complex argument z. A scaling option is available to help avoid overflow
**Source:** `zbiry.f`

### ZBKNU
**Purpose:** Subsidiary to ZAIRY, ZBESH, ZBESI and ZBESK
**Source:** `zbknu.f`

### ZBUNI
**Purpose:** Subsidiary to ZBESI and ZBESK
**Source:** `zbuni.f`

### ZBUNK
**Purpose:** Subsidiary to ZBESH and ZBESK
**Source:** `zbunk.f`

### ZDIV
**Purpose:** Subsidiary to ZBESH, ZBESI, ZBESJ, ZBESK, ZBESY, ZAIRY and ZBIRY
**Source:** `zdiv.f`

### ZEXP
**Purpose:** Subsidiary to ZBESH, ZBESI, ZBESJ, ZBESK, ZBESY, ZAIRY and ZBIRY
**Source:** `zexp.f`

### ZKSCL
**Purpose:** Subsidiary to ZBESK
**Source:** `zkscl.f`

### ZLOG
**Purpose:** Subsidiary to ZBESH, ZBESI, ZBESJ, ZBESK, ZBESY, ZAIRY and ZBIRY
**Source:** `zlog.f`

### ZMLRI
**Purpose:** Subsidiary to ZBESI and ZBESK
**Source:** `zmlri.f`

### ZMLT
**Purpose:** Subsidiary to ZBESH, ZBESI, ZBESJ, ZBESK, ZBESY, ZAIRY and ZBIRY
**Source:** `zmlt.f`

### ZRATI
**Purpose:** Subsidiary to ZBESH, ZBESI and ZBESK
**Source:** `zrati.f`

### ZS1S2
**Purpose:** Subsidiary to ZAIRY and ZBESK
**Source:** `zs1s2.f`

### ZSERI
**Purpose:** Subsidiary to ZBESI and ZBESK
**Source:** `zseri.f`

### ZSHCH
**Purpose:** Subsidiary to ZBESH and ZBESK
**Source:** `zshch.f`

### ZSQRT
**Purpose:** Subsidiary to ZBESH, ZBESI, ZBESJ, ZBESK, ZBESY, ZAIRY and ZBIRY
**Source:** `zsqrt.f`

### ZUCHK
**Purpose:** Subsidiary to SERI, ZUOIK, ZUNK1, ZUNK2, ZUNI1, ZUNI2 and ZKSCL
**Source:** `zuchk.f`

### ZUNHJ
**Purpose:** Subsidiary to ZBESI and ZBESK
**Source:** `zunhj.f`

### ZUNI1
**Purpose:** Subsidiary to ZBESI and ZBESK
**Source:** `zuni1.f`

### ZUNI2
**Purpose:** Subsidiary to ZBESI and ZBESK
**Source:** `zuni2.f`

### ZUNIK
**Purpose:** Subsidiary to ZBESI and ZBESK
**Source:** `zunik.f`

### ZUNK1
**Purpose:** Subsidiary to ZBESK
**Source:** `zunk1.f`

### ZUNK2
**Purpose:** Subsidiary to ZBESK
**Source:** `zunk2.f`

### ZUOIK
**Purpose:** Subsidiary to ZBESH, ZBESI and ZBESK
**Source:** `zuoik.f`

### ZWRSK
**Purpose:** Subsidiary to ZBESI and ZBESK
**Source:** `zwrsk.f`
