      SUBROUTINE DERV1(LABEL,VALUE,FLAG)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-10-20 DERV1  Krogh  Changes to use M77CON
C>> 1994-04-20 DERV1  CLL Edited to make DP & SP files similar.
C>> 1985-09-20 DERV1  Lawson  Initial code.
c--D replaces "?": ?ERV1
C
C     ------------------------------------------------------------
C     SUBROUTINE ARGUMENTS
C     --------------------
C     LABEL     An identifing name to be printed with VALUE.
C
C     VALUE     A floating point number to be printed.
C
C     FLAG      See write up for FLAG in ERMSG.
C
C     ------------------------------------------------------------
C
      COMMON/M77ERR/IDELTA,IALPHA
      INTEGER IDELTA,IALPHA
      DOUBLE PRECISION VALUE
      CHARACTER*(*) LABEL
      CHARACTER*1 FLAG
      SAVE /M77ERR/
C
      IF (IALPHA.GE.-1) THEN
        WRITE (*,*) '  ',LABEL,' = ',VALUE
        IF (FLAG.EQ.'.') CALL ERFIN
      ENDIF
      RETURN
C
      END
