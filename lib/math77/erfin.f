      SUBROUTINE ERFIN
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-11-11 CLL Typing all variables.
C>> 1985-09-23 ERFIN  Lawson  Initial code.
C
      integer idelta, ialpha
      COMMON/M77ERR/IDELTA,IALPHA
      SAVE /M77ERR/
C
 1003 FORMAT(1X,72('$')/' ')
      PRINT 1003
      IF (IALPHA.GE.2) STOP
      RETURN
      END
