# SLATEC Migration Master Map - Print Edition

This consolidated map integrates all migration information into print-friendly diagrams that show all 1,441 functions while being readable on standard paper sizes.

## Executive Summary

- **Total Functions**: 1,441
- **Completed**: 7 (PYTHAG, CDIV, I1MACH, R1MACH, D1MACH, ENORM, LSAME)
- **Zero Dependencies**: 338 (can migrate immediately)
- **Critical Path**: Error handling core (4 functions) enables 700+ dependents

## Master Dependency Flow (Page 1 of 2)

```mermaid
%%{init: {'theme': 'base', 'themeVariables': { 'fontSize': '10px'}}}%%
graph TB
    %% Styles
    classDef completed fill:#90EE90,stroke:#228B22,stroke-width:3px
    classDef critical fill:#FF6B6B,stroke:#8B0000,stroke-width:4px
    classDef available fill:#E0E0E0,stroke:#696969,stroke-width:2px
    classDef level1 fill:#ADD8E6,stroke:#4682B4,stroke-width:2px
    classDef level2 fill:#DDA0DD,stroke:#8B008B,stroke-width:2px
    classDef testGroup fill:#FFE4E1,stroke:#DC143C,stroke-width:2px,stroke-dasharray: 5 5
    
    %% Header
    subgraph "FOUNDATION LAYER - 836 Dependents"
        subgraph "Machine Constants"
            I1MACH[I1MACH<br/>836 deps]:::completed
            R1MACH[R1MACH<br/>325 deps]:::completed  
            D1MACH[D1MACH<br/>210 deps]:::completed
        end
        
        subgraph "Error Core - CRITICAL PATH"
            J4SAVE[J4SAVE<br/>742 deps]:::critical
            FDUMP[FDUMP<br/>739 deps]:::critical
            XERCNT[XERCNT<br/>731 deps]:::critical
            XERHLT[XERHLT<br/>731 deps]:::critical
        end
    end
    
    %% Error Chain
    subgraph "ERROR SYSTEM - Enables All Tests"
        XGETUA[XGETUA]:::level1
        XERSVE[XERSVE]:::level1
        XERPRN[XERPRN]:::level2
        XERMSG[XERMSG]:::level2
        XERBLA[XERBLA]:::level2
        
        J4SAVE --> XGETUA
        J4SAVE --> XERSVE
        I1MACH --> XERPRN
        J4SAVE --> XERPRN
        XGETUA --> XERPRN
        FDUMP --> XERMSG
        I1MACH --> XERMSG
        J4SAVE --> XERMSG
        XERCNT --> XERMSG
        XERHLT --> XERMSG
        XERPRN --> XERMSG
        XERSVE --> XERMSG
        XGETUA --> XERMSG
        XERMSG --> XERBLA
    end
    
    %% BLAS Foundation
    subgraph "BLAS LEVEL 1 - 99 Dependents Each"
        %% Most critical BLAS functions
        ISAMAX[ISAMAX<br/>99 deps]:::available
        SAXPY[SAXPY<br/>82 deps]:::available
        IDAMAX[IDAMAX<br/>81 deps]:::available
        DAXPY[DAXPY<br/>77 deps]:::available
        SDOT[SDOT<br/>66 deps]:::available
        DDOT[DDOT<br/>65 deps]:::available
        SCOPY[SCOPY<br/>64 deps]:::available
        SNRM2[SNRM2<br/>62 deps]:::available
        DCOPY[DCOPY<br/>57 deps]:::available
        DNRM2[DNRM2<br/>55 deps]:::available
        
        %% Additional BLAS
        SASUM:::available
        SSCAL:::available
        SSWAP:::available
        SROT:::available
        SROTG:::available
        DASUM:::available
        DSCAL:::available
        DSWAP:::available
        DROT:::available
        DROTG:::available
    end
    
    %% Test Programs
    subgraph "TEST PROGRAMS"
        TEST17[test17: BLAS L1]:::testGroup
        TEST18[test18: BLAS L2/3-S]:::testGroup
        TEST19[test19: BLAS L2/3-D]:::testGroup
        TEST10[test10: Complex]:::testGroup
        TEST35[test35: Nonlinear-S]:::testGroup
        TEST36[test36: Nonlinear-D]:::testGroup
    end
    
    %% Test Dependencies
    XERBLA -.->|enables| TEST17
    XERBLA -.->|enables| TEST18
    XERBLA -.->|enables| TEST19
    LSAME[LSAME]:::completed
    LSAME -.->|enables| TEST17
    LSAME -.->|enables| TEST18
    LSAME -.->|enables| TEST19
```

## Master Dependency Flow (Page 2 of 2)

```mermaid
%%{init: {'theme': 'base', 'themeVariables': { 'fontSize': '10px'}}}%%
graph TB
    %% Styles
    classDef completed fill:#90EE90,stroke:#228B22,stroke-width:3px
    classDef available fill:#E0E0E0,stroke:#696969,stroke-width:2px
    classDef level1 fill:#ADD8E6,stroke:#4682B4,stroke-width:2px
    classDef level2 fill:#DDA0DD,stroke:#8B008B,stroke-width:2px
    classDef level3 fill:#F0E68C,stroke:#DAA520,stroke-width:2px
    
    %% Complex Arithmetic
    subgraph "COMPLEX ARITHMETIC [Level 0]"
        CDIV[CDIV]:::completed
        CARG:::available
        CCOSH:::available
        CSINH:::available
        CTAN:::available
        CLOG10:::available
        ZABS:::available
        ZEXP:::available
        ZMLT:::available
        ZSHCH:::available
        CSHCH:::available
    end
    
    %% Norms
    subgraph "VECTOR NORMS [Level 0]"
        ENORM[ENORM]:::completed
        DENORM:::available
        HVNRM:::available
        DHVNRM:::available
        VNWRMS:::available
        DVNRMS:::available
        SDANRM:::available
        DDANRM:::available
    end
    
    %% Matrix Operations
    subgraph "MATRIX OPERATIONS [Level 0]"
        R1MPYQ:::available
        D1MPYQ:::available
        RWUPDT:::available
        DWUPDT:::available
        SGTSL:::available
        DGTSL:::available
        CGTSL:::available
        SPTSL:::available
        DPTSL:::available
        CPTSL:::available
    end
    
    %% Special Functions
    subgraph "SPECIAL FUNCTIONS"
        subgraph "Airy [Level 0]"
            JAIRY:::available
            DJAIRY:::available
            YAIRY:::available
            DYAIRY:::available
        end
        
        subgraph "Gamma [Mixed Levels]"
            GAMLIM[GAMLIM L0]:::available
            R9LGMC[R9LGMC L2]:::level2
            GAMMA[GAMMA L3]:::level3
            ALNGAM[ALNGAM L3]:::level3
        end
        
        subgraph "Bessel [Mixed Levels]"
            ASYIK[ASYIK L2]:::level2
            ASYJY[ASYJY L2]:::level2
            BESI[BESI L3]:::level3
            BESJ[BESJ L3]:::level3
        end
    end
    
    %% Quadrature
    subgraph "QUADRATURE [Level 0 Base]"
        QWGTC:::available
        QWGTF:::available
        QWGTS:::available
        DQWGTC:::available
        DQWGTF:::available
        DQWGTS:::available
    end
```

## Complete Function Inventory by Category

### Zero-Dependency Functions (338 total) - Can Migrate Now

| Category | Count | Key Functions | Priority |
|----------|-------|---------------|----------|
| **Error Core** | 4 | FDUMP, J4SAVE, XERCNT, XERHLT | 🔴 CRITICAL |
| **BLAS Level 1** | 38 | ISAMAX, SAXPY, SDOT, DAXPY, DDOT | 🔴 HIGH |
| **Complex Arithmetic** | 15 | CARG, CCOSH, ZABS, ZEXP, ZMLT | 🟡 MEDIUM |
| **Vector Norms** | 12 | DENORM, HVNRM, VNWRMS, SDANRM | 🟡 MEDIUM |
| **Matrix Direct Solvers** | 6 | SGTSL, DGTSL, SPTSL, DPTSL | 🟡 MEDIUM |
| **Matrix Updates** | 4 | R1MPYQ, D1MPYQ, RWUPDT, DWUPDT | 🟡 MEDIUM |
| **Airy Functions** | 4 | JAIRY, DJAIRY, YAIRY, DYAIRY | 🟢 LOW |
| **Quadrature Weights** | 6 | QWGTC, QWGTF, QWGTS, DQW* | 🟢 LOW |
| **Utilities** | 249 | PIMACH, various helpers | 🟢 LOW |

### Level 1 Dependencies (285 total) - Available After Core

| Category | Count | Key Functions | Depends On |
|----------|-------|---------------|------------|
| **BLAS Level 2** | 24 | SGEMV, DGEMV, SGER, DGER | BLAS L1 + XERBLA |
| **LU Factorization** | 12 | SGEFA, DGEFA, CGEFA | BLAS L1 |
| **Error Handlers** | 5 | XGETUA, XERSVE, XERCLR | J4SAVE |
| **Special Functions** | 44 | Various gamma, beta helpers | Machine constants |
| **Linear Solvers** | 200 | Various specialized solvers | BLAS + Error |

### Dependency Chains Summary

```
Level 0 (338) → Level 1 (285) → Level 2 (198) → Level 3 (174) → Level 4 (161) → ... → Level 10 (1)
```

## Migration Execution Plan

### Week 1: Critical Foundation (8 functions)
```
Error Core (4): FDUMP, J4SAVE, XERCNT, XERHLT
BLAS Core (4): ISAMAX, SAXPY, IDAMAX, DAXPY
→ Enables: 700+ functions, basic testing
```

### Week 2: Error System + Extended BLAS (15 functions)
```
Error Chain (5): XGETUA, XERSVE, XERPRN, XERMSG, XERBLA
BLAS Extension (10): SDOT, DDOT, SCOPY, DCOPY, SNRM2, DNRM2, SSCAL, DSCAL, SASUM, DASUM
→ Enables: Full BLAS testing (test17-19)
```

### Week 3: Complex & Norms (20 functions)
```
Complex (10): ZABS, ZEXP, ZMLT, CARG, CCOSH, CSINH, CTAN, CLOG10, ZSHCH, CSHCH
Norms (10): DENORM, HVNRM, DHVNRM, VNWRMS, DVNRMS, SDANRM, DDANRM, etc.
→ Enables: Complex arithmetic tests (test10), ODE solvers
```

### Week 4: Matrix Operations (20 functions)
```
Direct Solvers (6): SGTSL, DGTSL, CGTSL, SPTSL, DPTSL, CPTSL
Matrix Updates (4): R1MPYQ, D1MPYQ, RWUPDT, DWUPDT
Remaining BLAS L1 (10): SROT, DROT, SSWAP, DSWAP, etc.
→ Enables: Nonlinear solver tests (test35-36)
```

### Weeks 5-8: Systematic Coverage
- Complete remaining BLAS Level 1 (18 functions)
- Special functions foundation (40 functions)
- Quadrature base functions (20 functions)
- Additional utilities as needed

## Test Coverage Matrix

| Test Program | Required Functions | Status |
|--------------|-------------------|--------|
| test17 (BLAS L1) | LSAME ✓, XERBLA, BLAS L1 functions | Partial |
| test18 (BLAS L2/3-S) | LSAME ✓, XERBLA, BLAS L2 | Blocked |
| test19 (BLAS L2/3-D) | LSAME ✓, XERBLA, BLAS L2 | Blocked |
| test10 (Complex) | CDIV ✓, ZABS, ZEXP, ZMLT | Partial |
| test35 (Nonlinear-S) | ENORM ✓, R1MPYQ, RWUPDT | Partial |
| test36 (Nonlinear-D) | DENORM, D1MPYQ, DWUPDT | Blocked |

## Function Naming Convention Reference

| Prefix | Type | Example |
|--------|------|---------|
| S* | Single precision real | SAXPY |
| D* | Double precision real | DAXPY |
| C* | Single precision complex | CAXPY |
| Z* | Double precision complex | ZAXPY |
| I* | Integer operation | ISAMAX |
| R* | Real utility | R1MACH |
| X* | Error handling | XERMSG |

## Key Migration Insights

1. **Critical Path**: Error handling core (4 functions) → Error chain (5 functions) → XERBLA enables most testing
2. **High ROI**: First 20 functions enable 700+ dependents and 3 major test suites
3. **Parallel Work**: After error core, BLAS/Complex/Norms can be done in parallel
4. **Test-Driven**: Each phase enables specific test programs for validation
5. **No Circular Dependencies**: Clean acyclic dependency graph throughout

## Notes for Implementation

- Functions in same family (S*/D*/C*/Z* variants) share algorithms - migrate together
- Level 0 functions have no dependencies - can be migrated in any order within priority
- Use completed functions as templates for similar ones
- Run relevant test programs after each phase to validate migrations

---
*This document consolidates MIGRATION_MAP.md, COMPLETE_MIGRATION_MAP.md, and MIGRATION_GUIDE.md into a unified, print-optimized reference.*