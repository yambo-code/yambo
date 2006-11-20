      FUNCTION R1MACH (I) 
      IMPLICIT NONE
      INTEGER :: I
      REAL :: B, X = 1.0, r1mach

      B = RADIX(X)

      SELECT CASE (I)
        CASE (1) 
          R1MACH = TINY(X)            ! smallest positive magnitude.
        CASE (2)
          R1MACH = HUGE(X)            ! largest magnitude.
        CASE (3)
          R1MACH = B**(-DIGITS(X))    ! smallest relative spacing.
        CASE (4)
          R1MACH = B**(1-DIGITS(X))   ! largest relative spacing.
        CASE (5)
          R1MACH = LOG10(B)
        CASE DEFAULT
          STOP 'R1MACH -- input argument out of bounds'
      END SELECT

      RETURN
      END FUNCTION R1MACH



