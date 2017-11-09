      SUBROUTINE ERMSG(SUBNAM,INDIC,LEVEL,MSG,FLAG)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1995-11-22 ERMSG  Krogh Got rid of multiple entries.
c>> 1995-09-15 ERMSG  Krogh Remove '0' in format.
C>> 1994-11-11 ERMSG  Krogh   Declared all vars.
C>> 1992-10-20 ERMSG  WV Snyder  added ERLSET, ERLGET
C>> 1985-09-25 ERMSG  Lawson  Initial code.
C
C     --------------------------------------------------------------
C
C     Four entries: ERMSG, ERMSET, ERLGET, ERLSET
C     ERMSG initiates an error message. This subr also manages the
C     saved value IDELOC and the saved COMMON block M77ERR to
C     control the level of action. This is intended to be the
C     only subr that assigns a value to IALPHA in COMMON.
C     ERMSET resets IDELOC & IDELTA.  ERLGET returns the last value
C     of LEVEL passed to ERMSG.  ERLSET sets the last value of LEVEL.
C     ERLSET and ERLGET may be used together to determine the level
C     of error that occurs during execution of a routine that uses
C     ERMSG.
C
C     --------------------------------------------------------------
C     SUBROUTINE ARGUMENTS
C     --------------------
C     SUBNAM   A name that identifies the subprogram in which
C              the error occurs.
C
C     INDIC    An integer printed as part of the mininal error
C              message. It together with SUBNAM can be used to
C              uniquely identify an error.
C
C     LEVEL    The user sets LEVEL=2,0,or -2 to specify the
C              nominal action to be taken by ERMSG. The
C              subroutine ERMSG contains an internal variable
C              IDELTA, whose nominal value is zero. The
C              subroutine will compute IALPHA = LEVEL + IDELTA
C              and proceed as follows:
C              If (IALPHA.GE.2)        Print message and STOP.
C              If (IALPHA=-1,0,1)      Print message and return.
C              If (IALPHA.LE.-2)       Just RETURN.
C
C     MSG      Message to be printed as part of the diagnostic.
C
C     FLAG     A single character,which when set to '.' will
C              call the subroutine ERFIN and will just RETURN
C              when set to any other character.
C
C     --------------------------------------------------------------
C
C     C.Lawson & S.Chan, JPL, 1983 Nov
C
C     ------------------------------------------------------------------
      INTEGER IDELOC, LEVEL, IDELTA, IALPHA, INDIC
      COMMON /M77ERR/ IDELTA,IALPHA
      CHARACTER*(*) SUBNAM,MSG
      CHARACTER*1 FLAG
      SAVE /M77ERR/, IDELOC
      DATA IDELOC / 0 /
 1001 FORMAT(1X/' ',72('$')/' SUBPROGRAM ',A,' REPORTS ERROR NO. ',I4)
c
      if (LEVEL .lt. -1000) then
c                            Setting a new IDELOC.
        IDELTA = LEVEL + 10000
        IDELOC = IDELTA
        return
      end if
      IDELTA = IDELOC
      IALPHA = LEVEL + IDELTA
      IF (IALPHA.GE.-1) THEN
c
c            Setting FILE = 'CON' works for MS/DOS systems.
c
c
        WRITE (*,1001) SUBNAM,INDIC
        WRITE (*,*) MSG
        IF (FLAG.EQ.'.') CALL ERFIN
      END IF
      RETURN
C
      end
C
      subroutine ERMSET(IDEL)
      integer IDEL
c                            Call ERMSG to set IDELTA and IDELOC
      call ERMSG(' ', 0,IDEL-10000,' ',' ')
      RETURN
      END
