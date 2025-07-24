C     Stub implementation of XERMSG for validation purposes
      SUBROUTINE XERMSG (LIBRAR, SUBROU, MESSG, NERR, LEVEL)
      CHARACTER*(*) LIBRAR, SUBROU, MESSG
      INTEGER NERR, LEVEL
C     
C     Just print the error and continue (don't stop execution)
      PRINT *, 'XERMSG: ', TRIM(LIBRAR), '/', TRIM(SUBROU), ': ', 
     +         TRIM(MESSG), ' (Error ', NERR, ', Level ', LEVEL, ')'
      RETURN
      END