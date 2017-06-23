      subroutine AMACH(MODE, I, I1, R1, D1)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2003-04-25 AMACH  Krogh   Added comment for the Absoft compiler.
c>> 2001-03-13 AMACH  Krogh   Fixed HOW for case when not set in m77job.
c>> 1997-04-16 AMACH  Krogh   Remove blank lines -- was confusing m77con
c>> 1996-03-30 AMACH  Krogh   Added external statement.
c>> 1994-10-26 AMACH  Krogh   Changes to use M77CON
c>> 1994-09-23 AMACH  Snyder  Add VAX G parameters
c>> 1994-06-21 AMACH  Snyder  Compute only round-off and u-flow at first
c>> 1994-05-25 AMACH  Snyder  Added an option to compute at run time.
c>> 1992-04-07 AMACH  Oken    Removed ^Z at EOF (error found by VAX comp
c>> 1992-02-20 AMACH  Snyder  Added Cray-J90 stuff, q.v.
c>> 1990-06-11 AMACH  Snyder  Added Apollo DN-10000 stuff, q.v.
c>> 1990-12-14 AMACH  Lawson  Changed to eliminate ENTRY statements.
c>> 1990-08-21 AMACH  Krogh   No test was getting done for bad machine.
c>> 1990-02-28 AMACH  Krogh   Correct missing DOUBLE PRECISION AMSUB1
c>> 1989-08-14 AMACH  Krogh   Parameterized everything -- Massive change
c>> 1989-03-30 AMACH  Snyder  Correct missing "/" line 921
c>> 1989-01-30 AMACH  Snyder  Incorporate more constants from NETLIB.
C>> 1988-05-19 AMACH  Lawson  Initial code.
c File AMACH.FOR contains user-callable functions I1MACH, D1MACH, and
c R1MACH, plus second-level subroutines AMACH, AMTEST, and AMSUB1.
c Appropriate lines must be switched between comment and non-comment
c status when this code is moved to a different computer system.
c     These changes can be done with any text editor, however the "c++"
c lines permit automation of the change using the M77CON processor.
c Note that when the M77CON processor activates a line it shifts
c Columns 2-72 to 1-71 and puts a blank in Column 72.  When it inactiv-
c ates a line it shifts Columns 1-71 to 2-72 and puts a C in Column 1.
c     The possible choices using M77CON (don't include parenthetical
c     comments) are:
c      c++ CURRENT HAS SYS = IEEE
c      c++ CURRENT HAS SYS = ALPHA_D3
c      c++ CURRENT HAS SYS = AMDAHL
c      c++ CURRENT HAS SYS = APOLLO_10000
c      c++ CURRENT HAS SYS = BUR1700
c      c++ CURRENT HAS SYS = BUR5700
c      c++ CURRENT HAS SYS = BUR67_7700
c      c++ CURRENT HAS SYS = CDC60_7000
c      c++ CURRENT HAS SYS = CONVEXC_1
c      c++ CURRENT HAS SYS = CRAY1
c      c++ CURRENT HAS SYS = CRAY1_SD (Sngl prec.arith. used for dble.)
c      c++ CURRENT HAS SYS = CRAY1_64 (64 bit integers)
c      c++ CURRENT HAS SYS = CRAY1_SD_64 (64 bit int, SP used for DP)
c      c++ CURRENT HAS SYS = CRAY_T3D
c      c++ CURRENT HAS SYS = CRAY_J90
c      c++ CURRENT HAS SYS = CRAY_J90_SD (Sngl prec. used for dble.)
c      c++ CURRENT HAS SYS = DG_S2000
c      c++ CURRENT HAS SYS = HARRIS220
c      c++ CURRENT HAS SYS = HON600_6000
c      c++ CURRENT HAS SYS = HON_DPS_8_70
c      c++ CURRENT HAS SYS = HP700Q
c      c++ CURRENT HAS SYS = IBM360_370
c      c++ CURRENT HAS SYS = INTERDATA_8_32
c      c++ CURRENT HAS SYS = PDP10_KA
c      c++ CURRENT HAS SYS = PDP10_KB
c      c++ CURRENT HAS SYS = PDP11
c      c++ CURRENT HAS SYS = PRIME50
c      c++ CURRENT HAS SYS = SEQ_BAL_8000
c      c++ CURRENT HAS SYS = UNIVAC
c      c++ CURRENT HAS SYS = VAX
c      c++ CURRENT HAS SYS = VAX_G
c     The current choice is:
c++ CURRENT HAS SYS = IEEE
c
c     One can also select whether floating point constants are created
c     by the compiler or created at run time.  The choices using M77CON
c     are:
c      c++ CURRENT HAS HOW = COMPILER
c      c++ CURRENT HAS HOW = RUN
c
c If this is not set in m77job, it is set as follows:
c++((SYS==VAX)|(SYS==ALPHA_D3)) Default HOW = RUN
c++ Default HOW = COMPILER
c
c     The current choice is:
c++ CURRENT HAS HOW = COMPILER
c
c     If the constants are created at run time, and they fail the run-
c     time check for reasonableness, they are re-created assuming IEEE.
c     If they still fail, the program stops.
c
C  I/O UNIT NUMBERS:
C
C    IM1 = I1MACH( 1) = THE STANDARD INPUT UNIT.
C    IM2 = I1MACH( 2) = THE STANDARD OUTPUT UNIT.
C    IM3 = I1MACH( 3) = THE STANDARD PUNCH UNIT.
C    IM4 = I1MACH( 4) = THE STANDARD ERROR MESSAGE UNIT.
C
C  WORDS:
C
C    IM5 = I1MACH( 5) = THE NUMBER OF BITS PER INTEGER STORAGE UNIT.
C    IM6 = I1MACH( 6) = THE NUMBER OF CHARACTERS/INTEGER STORAGE UNIT.
C
C  INTEGERS:
C
C    ASSUME INTEGERS ARE REPRESENTED IN THE S-DIGIT, BASE-A FORM
C
C               SIGN ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) )
C
C               WHERE 0 .LE. X(I) .LT. A FOR I=0,...,S-1.
C
C    IM7 = I1MACH( 7) = A, THE BASE.
C    IM8 = I1MACH( 8) = S, THE NUMBER OF BASE-A DIGITS.
C    IM9 = I1MACH( 9) = A**S - 1, THE LARGEST MAGNITUDE.
C
C  FLOATING-POINT NUMBERS:
C
C    ASSUME FLOATING-POINT NUMBERS ARE REPRESENTED IN THE T-DIGIT,
C    BASE-B FORM
C
C               SIGN (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
C
C               WHERE 0 .LE. X(I) .LT. B FOR I=1,...,T,
C               0 .LT. X(1), AND EMIN .LE. E .LE. EMAX.
C
C    IM10 = I1MACH(10) = B, THE BASE.
C
C  SINGLE-PRECISION:
C
C    IM11 = I1MACH(11) = T, THE NUMBER OF BASE-B DIGITS.
C    IM12 = I1MACH(12) = EMIN, THE SMALLEST EXPONENT E.
C    IM13 = I1MACH(13) = EMAX, THE LARGEST EXPONENT E.
C
C  DOUBLE-PRECISION:
C
C    IM14 = I1MACH(14) = T, THE NUMBER OF BASE-B DIGITS.
C    IM15 = I1MACH(15) = EMIN, THE SMALLEST EXPONENT E.
C    IM16 = I1MACH(16) = EMAX, THE LARGEST EXPONENT E.
C
C  CONVERSION FROM FUNCTIONAL TO STRUCTURAL FLOATING POINT CONSTANTS
C
C    IM17 = CONSTANT SUCH THAT IM14 + IM17 = ACTUAL NUMBER OF BASE-B
C           DIGITS IN DOUBLE PRECISION, USED FOR CHECKING THAT CORRECT
C           VERSION OF THIS PROGRAM IS INSTALLED.  (SEE DEFINITION OF
C           DM6, AND THE USE OF DM6 IN CALLING AMTEST.)
C
C  TO ALTER THIS FUNCTION FOR A PARTICULAR ENVIRONMENT,
C  THE DESIRED SET OF PARAMETER STATEMENTS SHOULD BE ACTIVATED BY
C  REMOVING THE C FROM COLUMN 1.  ALSO, THE VALUES OF
C  IM1 - IM4 SHOULD BE CHECKED FOR CONSISTENCY
C  WITH THE LOCAL OPERATING SYSTEM.
c     -----------------------------------------------------------------
c     Original design and code due to P. A. Fox, A. D. Hall, and
c     N. L. Schryer, Bell Laboratories.  See ACM TOMS, 4,(1978),177-188.
c     Adapted to Univac 1100 by Kris Stewart, JPL, 7/30/81.
c     Adapted for the JPL MATH77 library by C. L. Lawson and F. T. Krogh
c     Sept, 1987.
c     1989-08-14 AMACH  Krogh   Parameterized everything. Major changes.
C     1990 Dec. CLL reorganized code to avoid using ENTRY statements
c     for functions of different types.  Also added save statements.
c     -----------------------------------------------------------------
c     On the first call to this function, tests are done to verify that
c     IM10 and IM14 are not grossly wrong for the host environment.
c     This gives some protection against using the wrong version of this
c     subprogram.
c     -----------------------------------------------------------------
      integer MODE, I, I1
      real R1
      double precision D1, TEST
c
      integer IMACH(17)
      integer IM1, IM2, IM3, IM4, IM5, IM6, IM7, IM8, IM9, IM10, IM11,
     1            IM12, IM13, IM14, IM15, IM16, IM17
c++ Code for HOW=RUN is INACTIVE
C      integer IEEE
C      integer ID1, ID2, ID3, ID4, ID5, ID6, ID7, ID8, ID10, ID11,
C     1   ID12, ID13, ID14, ID15, ID16, ID17
c++ Code for (HOW=RUN) | SYS=IEEE is ACTIVE
      integer IE1, IE2, IE3, IE4, IE5, IE6, IE7, IE8, IE10, IE11,
     1   IE12, IE13, IE14, IE15, IE16, IE17
c++ end
      real             RMACH(5), RM1, RM2, RM3, RM4, RM5,
     1                 RMA, RMB, RBASE
      double precision DMACH(5), DM1, DM2, DM3, DM4, DM5, DM6,
     1                 DMA, DMB, DBASE
      save TEST, IMACH, RMACH, DMACH
C     -----------------------------------------------------------------
C     Machine constants for IEEE standard binary floating-point
c     processors.  This includes PC's and work-stations using the
c     Intel 8087, 80287, 80387, ... processors or the
c     Motorola 68881, 68882, ... processors.
c     Note:  We are setting the "most negative exponent" (IMACH(12) and
c     IMACH(15)) to be the exponent of the smallest normalized number.
c     An IEEE processor actually handles smaller numbers before
c     underflowing, however these "unnormalized" numbers have
c     diminished precision.
c
c++ Code for (HOW=RUN) | SYS=IEEE is ACTIVE
c     Parameters for IEEE when generating at run time:
      PARAMETER (IE1 =5, IE2 =6, IE3 =7, IE4 =6)
      PARAMETER (IE5 =32, IE6 =4, IE7 =2, IE8 =31)
      PARAMETER (IE10 =2, IE11 =24, IE12 =-125, IE13 =128)
      PARAMETER (IE14 =53, IE15 =-1021, IE16 =1024, IE17=0)
c++ Code for SYS = IEEE is ACTIVE
      PARAMETER (IM1 = IE1, IM2 = IE2, IM3 = IE3, IM4 = IE4)
      PARAMETER (IM5 = IE5, IM6 = IE6, IM7 = IE7, IM8 = IE8)
      PARAMETER (IM10 = IE10, IM11 = IE11, IM12 = IE12, IM13 = IE13)
      PARAMETER (IM14 = IE14, IM15 = IE15, IM16 = IE16, IM17 = IE17)
C     -----------------------------------------------------------------
c++ Code for SYS = ALPHA_D3 is INACTIVE
Cc     MACHINE CONSTANTS for the VAX/VMS F and D-3 format for Alpha
Cc
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
C      PARAMETER (IM10 =2, IM11 =24, IM12 =-127, IM13 =127)
C      PARAMETER (IM14 =53, IM15 =-127, IM16 =127, IM17=0)
c++ end
C     -----------------------------------------------------------------
c++ Code for HOW = RUN is INACTIVE
Cc     MACHINE CONSTANTS for the VAX/VMS F and D-3 format for Alpha
Cc
C      PARAMETER (ID1 =5, ID2 =6, ID3 =7, ID4 =6)
C      PARAMETER (ID5 =32, ID6 =4, ID7 =2, ID8 =31)
C      PARAMETER (ID10 =2, ID11 =24, ID12 =-127, ID13 =127)
C      PARAMETER (ID14 =53, ID15 =-127, ID16 =127, ID17=0)
c++ end
C     -----------------------------------------------------------------
c++ Code for SYS = AMDAHL is INACTIVE
CC     MACHINE CONSTANTS FOR AMDAHL MACHINES.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
C      PARAMETER (IM10 =16, IM11 =6, IM12 =-64, IM13 =63)
C      PARAMETER (IM14 =14, IM15 =-64, IM16 =63, IM17=0)
C      -----------------------------------------------------------------
c++ Code for SYS = APOLLO_10000 is INACTIVE
cc     MACHINE CONSTANTS FOR APOLLO DN_10000 MACHINES.
cc     The only difference from IEEE is IM13.  This difference has
cc     nothing to do with the arithmetic or representation used by the
cc     machine.  It is caused by a bug in the compiler:  The right-hand
cc     side of RM2 (below) is apparently evaluated in double precision.
cc     When the compiler is ready to store the resulting value into its
cc     internal data structures, it compares it to an incorrect value
cc     of the overflow limit.  It appears the incorrect value has the
cc     correct exponent, but the fraction is 1.5 instead of 2-2**(-p),
cc     where p is the precision in bits.  You can get the correct result
cc     by changing IM13 to 128, changing RM2 from a parameter to a
cc     variable, and changing the parameter statement that assigns a
cc     value to RM2 into an ordinary assignment statement.
CC
c      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
c      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
c      PARAMETER (IM10 =2, IM11 =24, IM12 =-125, IM13 =127)
c      PARAMETER (IM14 =53, IM15 =-1021, IM16 =1024, IM17 =0)
CC     -----------------------------------------------------------------
c++ Code for SYS = BUR1700 is INACTIVE
CC     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM.
CC
C      PARAMETER (IM1 =7, IM2 =2, IM3 =2, IM4 =2)
C      PARAMETER (IM5 =36, IM6 =4, IM7 =2, IM8 =33)
C      PARAMETER (IM10 =2, IM11 =24, IM12 =-256, IM13 =255)
C      PARAMETER (IM14 =60, IM15 =-256, IM16 =255, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = BUR5700 is INACTIVE
CC     MACHINE CONSTANTS FOR THE BURROUGHS 5700 SYSTEM.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =48, IM6 =6, IM7 =2, IM8 =39)
C      PARAMETER (IM10 =8, IM11 =13, IM12 =-50, IM13 =76)
C      PARAMETER (IM14 =26, IM15 =-50, IM16 =76, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = BUR67_7700 is INACTIVE
CC     MACHINE CONSTANTS FOR THE BURROUGHS 6700/7700 SYSTEMS.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =48, IM6 =6, IM7 =2, IM8 =39)
C      PARAMETER (IM10 =8, IM11 =13, IM12 =-50, IM13 =76)
C      PARAMETER (IM14 =26, IM15 =-32754, IM16 =32780, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = CDC60_7000 is INACTIVE
CC     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =60, IM6 =10, IM7 =2, IM8 =48)
C      PARAMETER (IM10 =2, IM11 =47, IM12 =-929, IM13 =1070)
C      PARAMETER (IM14 =94, IM15 =-929, IM16 =1069, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = CONVEXC_1 is INACTIVE
CC     MACHINE CONSTANTS FOR CONVEX C-1.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
C      PARAMETER (IM10 =2, IM11 =24, IM12 =-128, IM13 =127)
C      PARAMETER (IM14 =53, IM15 =-1024, IM16 =1023, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = CRAY1 is INACTIVE
CC     MACHINE CONSTANTS FOR THE CRAY 1, XMP, 2, AND 3.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =102, IM4 =6)
C      PARAMETER (IM5 =64, IM6 =8, IM7 =2, IM8 =46)
C      PARAMETER (IM10 =2, IM11 =47, IM12 =-8189, IM13 =8190)
C      PARAMETER (IM14 =94, IM15 =-8099, IM16 =8190, IM17=2)
CC     -----------------------------------------------------------------
c++ Code for SYS = CRAY_T3D is INACTIVE
cc     Machine constants for Cray T3D.  IEEE double for both precisions.
c      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
c      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
c      PARAMETER (IM10 =2, IM11 =53, IM12 =-1021, IM13 =1024)
c      PARAMETER (IM14 =53, IM15 =-1021, IM16 =1024, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = CRAY_J90 is INACTIVE
CC     MACHINE CONSTANTS FOR THE CRAY J90
CC     Cray claims the overflow exponent (IM13 and IM16) is 8189, and
CC     the underflow exponent (IM12 and IM15) is -8189, but these values
CC     don't seem to work in cf77:  the underflow limit underflows, and
CC     the overflow limit overflows when using Cray's values.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =102, IM4 =6)
C      PARAMETER (IM5 =64, IM6 =8, IM7 =2, IM8 =46)
C      PARAMETER (IM10 =2, IM11 =47, IM12 =-8188, IM13 =8189)
C      PARAMETER (IM14 =94, IM15 =-8188, IM16 =8189, IM17=2)
CC     -----------------------------------------------------------------
c++ Code for SYS = CRAY_J90_SD is INACTIVE
CC     MACHINE CONSTANTS FOR THE CRAY J90
CC     Cray claims the overflow exponent (IM13 and IM16) is 8189, and
CC     the underflow exponent (IM12 and IM15) is -8189, but these
CC     values don't seem to work in cf77:  the underflow limit under-
CC     flows, and the overflow limit overflows when using Cray's values.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =102, IM4 =6)
C      PARAMETER (IM5 =64, IM6 =8, IM7 =2, IM8 =46)
C      PARAMETER (IM10 =2, IM11 =47, IM12 =-8188, IM13 =8189)
C      PARAMETER (IM14 =47, IM15 =-8188, IM16 =8189, IM17=1)
CC     -----------------------------------------------------------------
c++ Code for SYS = CRAY1_SD is INACTIVE
CC     MACHINE CONSTANTS FOR THE CRAY 1, XMP, 2, AND 3, WHEN DOUBLE
CC     PRECISION IS TO USE SINGLE PRECISION ARITHMETIC.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =102, IM4 =6)
C      PARAMETER (IM5 =64, IM6 =8, IM7 =2, IM8 =46)
C      PARAMETER (IM10 =2, IM11 =47, IM12 =-8189, IM13 =8190)
C      PARAMETER (IM14 =47, IM15 =-8189, IM16 =8190, IM17=1)
CC     -----------------------------------------------------------------
c++ Code for SYS = CRAY1_64 is INACTIVE
CC     MACHINE CONSTANTS FOR THE CRAY 1, XMP, 2, AND 3.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =102, IM4 =6)
C      PARAMETER (IM5 =64, IM6 =8, IM7 =2, IM8 =63)
C      PARAMETER (IM10 =2, IM11 =47, IM12 =-8189, IM13 =8190)
C      PARAMETER (IM14 =94, IM15 =-8099, IM16 =8190, IM17=2)
CC     -----------------------------------------------------------------
c++ Code for SYS = CRAY1_SD_64 is INACTIVE
CC     MACHINE CONSTANTS FOR THE CRAY 1, XMP, 2, AND 3, WHEN DOUBLE
CC     PRECISION IS TO USE SINGLE PRECISION ARITHMETIC.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =102, IM4 =6)
C      PARAMETER (IM5 =64, IM6 =8, IM7 =2, IM8 =63)
C      PARAMETER (IM10 =2, IM11 =47, IM12 =-8189, IM13 =8190)
C      PARAMETER (IM14 =47, IM15 =-8189, IM16 =8190, IM17=1)
CC     -----------------------------------------------------------------
c++ Code for SYS = DG_S2000 is INACTIVE
CC     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200
CC
C      PARAMETER (IM1 =11, IM2 =12, IM3 =8, IM4 =10)
C      PARAMETER (IM5 =16, IM6 =2, IM7 =2, IM8 =15)
C      PARAMETER (IM10 =16, IM11 =6, IM12 =-64, IM13 =63)
C      PARAMETER (IM14 =14, IM15 =-64, IM16 =63, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = HARRIS220 is INACTIVE
CC     MACHINE CONSTANTS FOR THE HARRIS 220, SLASH 6, SLASH 7.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =0, IM4 =6)
C      PARAMETER (IM5 =24, IM6 =3, IM7 =2, IM8 =23)
C      PARAMETER (IM10 =2, IM11 =23, IM12 =-127, IM13 =127)
C      PARAMETER (IM14 =38, IM15 =-127, IM16 =127, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = HON600_6000 is INACTIVE
CC     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000 SERIES.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =43, IM4 =6)
C      PARAMETER (IM5 =36, IM6 =6, IM7 =2, IM8 =35)
C      PARAMETER (IM10 =2, IM11 =27, IM12 =-127, IM13 =127)
C      PARAMETER (IM14 =63, IM15 =-127, IM16 =127, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = HON_DPS_8_70 is INACTIVE
CC     MACHINE CONSTANTS FOR THE HONEYWELL DPS 8/70 SERIES.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =43, IM4 =6)
C      PARAMETER (IM5 =36, IM6 =4, IM7 =2, IM8 =35)
C      PARAMETER (IM10 =2, IM11 =27, IM12 =-127, IM13 =127)
C      PARAMETER (IM14 =63, IM15 =-127, IM16 =127, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = HP700Q is INACTIVE
cc     Machine constants for HP-700 using the +autodblpad option,
cc     which automatically increases DOUBLE PRECISION to REAL*16, and
cc     REAL to DOUBLE PRECISION.
c      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
c      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
c      PARAMETER (IM10 =2, IM11 =53, IM12 =-1021, IM13 =1024)
c      PARAMETER (IM14 = 113, IM15 = -16381, IM16 = 16384, IM17 = 0)
CC     -----------------------------------------------------------------
c++ Code for SYS = IBM360_370 is INACTIVE
CC     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
CC     THE XEROX SIGMA 5/7/9 AND THE SEL SYSTEMS 85/86.
Cc
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
C      PARAMETER (IM10 =16, IM11 =6, IM12 =-64, IM13 =63)
C      PARAMETER (IM14 =14, IM15 =-64, IM16 =63, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = INTERDATA_8_32 is INACTIVE
CC     MACHINE CONSTANTS FOR THE INTERDATA 8/32
CC     WITH THE UNIX SYSTEM FORTRAN 77 COMPILER.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =6, IM4 =6)
C      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
C      PARAMETER (IM10 =16, IM11 =6, IM12 =-64, IM13 =62)
C      PARAMETER (IM14 =14, IM15 =-64, IM16 =62, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = PDP10_KA is INACTIVE
CC     MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR).
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =36, IM6 =5, IM7 =2, IM8 =35)
C      PARAMETER (IM10 =2, IM11 =27, IM12 =-128, IM13 =127)
C      PARAMETER (IM14 =54, IM15 =-101, IM16 =127, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = PDP10_KB is INACTIVE
CC     MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR).
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =36, IM6 =5, IM7 =2, IM8 =35)
C      PARAMETER (IM10 =2, IM11 =27, IM12 =-128, IM13 =127)
C      PARAMETER (IM14 =62, IM15 =-128, IM16 =127, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = PDP11 is INACTIVE
CC     MACHINE CONSTANTS FOR PDP-11 FORTRAN'S SUPPORTING
CC     16-BIT INTEGER ARITHMETIC.
Cc
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =16, IM6 =2, IM7 =2, IM8 =15)
C      PARAMETER (IM10 =2, IM11 =24, IM12 =-127, IM13 =127)
C      PARAMETER (IM14 =56, IM15 =-127, IM16 =127, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = PRIME50 is INACTIVE
CC     MACHINE CONSTANTS FOR THE PRIME 50 SERIES SYSTEMS
CC     WITH 32-BIT INTEGERS AND 64V MODE INSTRUCTIONS,
CC     SUPPLIED BY IGOR BRAY.
Cc
C      PARAMETER (IM1 =1, IM2 =1, IM3 =2, IM4 =1)
C      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
C      PARAMETER (IM10 =2, IM11 =23, IM12 =-127, IM13 =127)
C      PARAMETER (IM14 =47, IM15 =-32895, IM16 =32637, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = SEQ_BAL_8000 is INACTIVE
CC     MACHINE CONSTANTS FOR THE SEQUENT BALANCE 8000.
CC
C      PARAMETER (IM1 =0, IM2 =0, IM3 =7, IM4 =0)
C      PARAMETER (IM5 =32, IM6 =1, IM7 =2, IM8 =31)
C      PARAMETER (IM10 =2, IM11 =24, IM12 =-125, IM13 =128)
C      PARAMETER (IM14 =53, IM15 =-1021, IM16 =1024, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = UNIVAC is INACTIVE
CC     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES.
CC
CC     NOTE THAT THE PUNCH UNIT, I1MACH(3), HAS BEEN SET TO 1
CC     WHICH IS APPROPRIATE FOR THE UNIVAC-FTN SYSTEM.
CC     IF YOU HAVE THE UNIVAC-FOR SYSTEM, SET IT TO 7.
CC     IM6 = 4 for FTN (4 chars per word), 6 for FOR (6 chars per word).
Cc
C      PARAMETER (IM1 =5, IM2 =6, IM3 =1, IM4 =6)
C      PARAMETER (IM5 =36, IM6 =4, IM7 =2, IM8 =35)
C      PARAMETER (IM10 =2, IM11 =27, IM12 =-128, IM13 =127)
C      PARAMETER (IM14 =60, IM15 =-1024, IM16 =1023, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = VAX is INACTIVE
Cc     MACHINE CONSTANTS for the VAX/VMS F and D formats
Cc     and for PDP-11 FORTRAN SUPPORTING 32-BIT INTEGER ARITHMETIC.
Cc
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
C      PARAMETER (IM10 =2, IM11 =24, IM12 =-127, IM13 =127)
C      PARAMETER (IM14 =56, IM15 =-127, IM16 =127, IM17=0)
c++ end
C     -----------------------------------------------------------------
c++ Code for SYS = VAX_G is INACTIVE
Cc     MACHINE CONSTANTS for the VAX/VMS F and G formats
Cc     and for PDP-11 FORTRAN SUPPORTING 32-BIT INTEGER ARITHMETIC.
Cc
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
C      PARAMETER (IM10 =2, IM11 =24, IM12 =-127, IM13 =127)
C      PARAMETER (IM14 =53, IM15 =-1023, IM16 =1023, IM17=0)
c++ end
C     -----------------------------------------------------------------
C
C
C Real parameters
C
C  RM1 = R1MACH(1) = B**(EMIN-1), The smallest positive number, i.e.,
c                    the underflow limit.
C  RM2 = R1MACH(2) = B**EMAX*(1 - B**(-T)), The largest number, i.e.,
c                    the overflow limit.
C  RM3 = R1MACH(3) = B**(-T), The smallest relative spacing, i.e., the
c                    difference between 1.0 and the next smaller number.
C  RM4 = R1MACH(4) = B**(1-T), The largest relative spacing, i.e., the
c                     difference between 1.0 and the next larger number.
C  RM5 = R1MACH(5) = LOG10(B).  When B = 2 this value is
c              Log10(2) = 0.30102_99956_63981_19521_37388_94724
C
C Parameter RMA and RMB are selected so that for values of the base =
C 2, 8, 16, 10, RMA has the values 1, 3, 4, 0, and RMB has the values 0,
C 0, 0, 1.  These values are used in computing RM5.
C $$$$ Note that if other bases are to be supported, the calculation of
C $$$$ RMA and RMB will have to be generalized.
C
c++   Code for HOW = COMPILER is ACTIVE
      PARAMETER (IM9 = 2 * (2**(IM8-1) - 1) + 1)
      PARAMETER (RMA = ((IM10 - 10) * (-3 + ((IM10 - 2) * (-77 +
     1    12 * (IM10 - 8))) / 14)) / 24)
      PARAMETER (RMB = ((IM10 - 2) * (IM10 - 8) * (16 - IM10)) / 96)
      PARAMETER (RBASE = IM10)
C
C     Weird subterfuges below are NECESSARY to compute DM1 and DM2 on
C     some systems.  DON'T SIMPLIFY THEM.  We compute RM1 and RM2 using
C     these subterfuges so it will be clear we're computing the REAL
C     and DOUBLE PRECISION characteristics in the same way.
      PARAMETER (RM1 = (RBASE**(IM12/2)) * (RBASE**(IM12-IM12/2-1)))
      PARAMETER (RM2 = RBASE**(IM13-IM11) * ((RBASE**IM11 - RBASE)
     1               + (RBASE - 1.0E0)))
      PARAMETER (RM3 = RBASE**(-IM11))
      PARAMETER (RM4 = RBASE**(1-IM11))
c     PARAMETER (RM5 = RMA*0.30102 99956 63981 19521 37388 94724E0+RMB)
      PARAMETER (RM5 = RMA*0.301029995663981195213738894724E0+RMB)
C
C Double precision parameters -- (Defined like the real ones.)
C
      PARAMETER (DMA = ((IM10 - 10) * (-3 + ((IM10 - 2) * (-77 +
     1    12 * (IM10 - 8))) / 14)) / 24)
      PARAMETER (DMB = ((IM10 - 2) * (IM10 - 8) * (16 - IM10)) / 96)
      PARAMETER (DBASE = IM10)
C
C     Weird subterfuges below are NECESSARY to compute DM1 and DM2 on
C     some systems.  DON'T SIMPLIFY THEM.
C      If you are using the Absoft Pro Fortran for Windows v8.0
C      Uncomment the following line and comment the one after.
C      PARAMETER (DM1=.222507385850721D-307)
      PARAMETER (DM1 = (DBASE**(IM15/2)) * (DBASE**(IM15-IM15/2-1)))
      PARAMETER (DM2 = DBASE**(IM16-IM14) * ((DBASE**IM14 - DBASE)
     1               + (DBASE - 1.0D0)))
      PARAMETER (DM3 = DBASE**(-IM14))
      PARAMETER (DM4 = DBASE**(1-IM14))
c     PARAMETER (DM5 = DMA *
c    1 0.30102 99956 63981 19521 37388 94724 49302 67681 89881 46211 D0
c    2 + DMB)
c
      PARAMETER (DM5 = DMA*
     1 0.30102999566398119521373889472449302676818988146211D0 + DMB)
C DM6 and TEST are used in checking that the correct constants have
C been selected.
      PARAMETER (DM6 = DBASE**(-IM14-IM17))
c++   END
      data TEST / 0.D0 /
C
c     DATA IMACH / IM1, IM2, IM3, IM4, IM5, IM6, IM7, IM8, IM9, IM10,
c    1   IM11, IM12, IM13, IM14, IM15, IM16 /
c     DATA RMACH / RM1, RM2, RM3, RM4, RM5 /
c     DATA DMACH / DM1, DM2, DM3, DM4, DM5 /
C     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      if (TEST .eq. 0.0D0) then
C         IM9 = 2 * (2**(IM8-1) - 1) + 1
         IMACH(1) = IM1
         IMACH(2) = IM2
         IMACH(3) = IM3
         IMACH(4) = IM4
         IMACH(5) = IM5
         IMACH(6) = IM6
         IMACH(7) = IM7
         IMACH(8) = IM8
         IMACH(10) = IM10
         IMACH(11) = IM11
         IMACH(12) = IM12
         IMACH(13) = IM13
         IMACH(14) = IM14
         IMACH(15) = IM15
         IMACH(16) = IM16
         IMACH(17) = IM17
c++   Code for HOW = RUN is INACTIVE
C         IEEE = 0
C100      continue
C      DBASE = IMACH(10)
CC
CC     Weird subterfuge below is NECESSARY to compute DM1 on
CC     some systems.  DON'T SIMPLIFY IT.
C      DM1=(DBASE**(IMACH(15)/2)) * (DBASE**(IMACH(15)-IMACH(15)/2-1))
CC DM6 and TEST are used in checking that the correct constants have
CC been selected.
C      DM6 = DBASE**(-IMACH(14)-IMACH(17))
c++   end
         CALL AMTEST (TEST, DM6)
         if (dm1 .eq. 0.0d0 .or. test .eq. 0.0d0) then
c++   Code for HOW = RUN is INACTIVE
C           if (IEEE .eq. 0) then
C              IEEE = 1
C              IMACH(1) = IE1
C              IMACH(2) = IE2
C              IMACH(3) = IE3
C              IMACH(4) = IE4
C              IMACH(5) = IE5
C              IMACH(6) = IE6
C              IMACH(7) = IE7
C              IMACH(8) = IE8
C              IMACH(10) = IE10
C              IMACH(11) = IE11
C              IMACH(12) = IE12
C              IMACH(13) = IE13
C              IMACH(14) = IE14
C              IMACH(15) = IE15
C              IMACH(16) = IE16
C              IMACH(17) = IE17
C              go to 100
C           end if
C           if (IEEE .eq. 1) then
C              IEEE = 2
C              IMACH(1) = ID1
C              IMACH(2) = ID2
C              IMACH(3) = ID3
C              IMACH(4) = ID4
C              IMACH(5) = ID5
C              IMACH(6) = ID6
C              IMACH(7) = ID7
C              IMACH(8) = ID8
C              IMACH(10) = ID10
C              IMACH(11) = ID11
C              IMACH(12) = ID12
C              IMACH(13) = ID13
C              IMACH(14) = ID14
C              IMACH(15) = ID15
C              IMACH(16) = ID16
C              IMACH(17) = ID17
C              go to 100
C           end if
c++   END
            print*,'AMACH has bad parameters for current environment.'
            stop
         end if
c++   Code for HOW = RUN is INACTIVE
C         IM9 = 2 * (2**(IMACH(8)-1) - 1) + 1
C         RMA = ((IMACH(10) - 10) * (-3 + ((IMACH(10) - 2) * (-77 +
C     1       12 * (IMACH(10) - 8))) / 14)) / 24
C         RMB = ((IMACH(10)-2) * (IMACH(10)-8) * (16-IMACH(10)))/96
C         RBASE = IMACH(10)
CC
CC        Weird subterfuges below are NECESSARY to compute DM1 and DM2
CC        on some systems.  DON'T SIMPLIFY THEM.  We compute RM1 and
CC        RM2 using these subterfuges so it will be clear we're
CC        computing the REAL and DOUBLE PRECISION characteristics in
Cc        the same way.
C         RM1=(RBASE**(IMACH(12)/2))*(RBASE**(IMACH(12)-IMACH(12)/2-1))
C         RM2 = RBASE**(IMACH(13)-IMACH(11))*((RBASE**IMACH(11) - RBASE)
C     1                  + (RBASE - 1.0E0))
C         RM3 = RBASE**(-IMACH(11))
C         RM4 = RBASE**(1-IMACH(11))
Cc        RM5 = RMA*0.30102 99956 63981 19521 37388 94724E0+RMB
C         RM5 = RMA*0.301029995663981195213738894724E0+RMB
CC
CC Double precision parameters -- (Defined like the real ones.)
CC
C         DMA = ((IMACH(10) - 10) * (-3 + ((IMACH(10) - 2) * (-77 +
C     1       12 * (IMACH(10) - 8))) / 14)) / 24
C         DMB = ((IMACH(10)-2) * (IMACH(10)-8) * (16-IMACH(10)))/96
CC
CC        Weird subterfuge below is NECESSARY to compute DM2 on
CC        some systems.  DON'T SIMPLIFY IT.
C         DM2 = DBASE**(IMACH(16)-IMACH(14))*((DBASE**IMACH(14) - DBASE)
C     1                  + (DBASE - 1.0D0))
C         DM3 = DBASE**(-IMACH(14))
C         DM4 = DBASE**(1-IMACH(14))
Cc        DM5 = DMA*0.30102 99956 63981 19521 37388 94724D0+DMB
C         DM5 = DMA*0.301029995663981195213738894724D0+DMB
c++   END
         IMACH(9) = IM9
         RMACH(1) = RM1
         RMACH(2) = RM2
         RMACH(3) = RM3
         RMACH(4) = RM4
         RMACH(5) = RM5
         DMACH(1) = DM1
         DMACH(2) = DM2
         DMACH(3) = DM3
         DMACH(4) = DM4
         DMACH(5) = DM5
      ENDIF
C
      if (MODE .eq. 0) then
         I1=IMACH(I)
      else if (MODE .eq. 1) then
         R1=RMACH(I)
c                                  Here we assume MODE = 2.
      else
         D1=DMACH(I)
      endif
      return
      end
c     ==================================================================
      integer function I1MACH(I)
      integer I, I1
      real R1
      double precision D1
      IF (I .LT. 1  .OR.  I .GT. 16) THEN
         PRINT*,'I1MACH.. Bad argument: I =',I
         STOP 'I1MACH error'
      END IF
      call AMACH (0, I, I1, R1, D1)
      I1MACH = I1
      return
      end
c     ==================================================================
c
      real function R1MACH(I)
      integer I, I1
      real R1
      double precision D1
      IF (I .lt. 1  .or.  I .gt. 5) THEN
         print*,'R1MACH.. Bad argument: I = ',I
         stop 'R1MACH error'
      END IF
      call AMACH (1, I, I1, R1, D1)
      R1MACH = R1
      RETURN
      end
c     ==================================================================
c
      double precision function D1MACH(I)
      integer I, I1
      real R1
      double precision D1
      IF (I .lt. 1  .or.  I .gt. 5) THEN
         print*,'D1MACH.. Bad argument: I = ',I
         stop 'D1MACH error'
      END IF
      call AMACH (2, I, I1, R1, D1)
      D1MACH = D1
      RETURN
      END
c     ==================================================================
c
      SUBROUTINE AMTEST (TEST, D6)
c Verifies that D6 is an appropriate value for DM6.
c Returns TEST = D6 + D6 - 1, .ne. 0 if D6 is an appropriate value for
c DM6, else returns TEST = 0.  The caller uses TEST = 0 as a signal to
c try again with IEEE settings (unless that's already been done).
      external AMSUB1
      DOUBLE PRECISION AMSUB1, D6, TEST
      TEST = AMSUB1(1.D0 + D6)
C
C The comparison with 1.875E0*D6 in the line below is to guard
C against the possibility that TEST is > 0 as a result of rounding
C up in the addition of D6 to 1.
C
      IF ((TEST .eq. 0.D0) .or. (TEST .gt. 1.875D0*D6)) THEN
         TEST = (D6 + D6) + 1.D0
         IF (AMSUB1(TEST) .ne. 0.D0) RETURN
      END IF
      test = 0.0d0
      END
c     ==================================================================
c
      DOUBLE PRECISION FUNCTION AMSUB1 (TEST1)
      DOUBLE PRECISION TEST1
C     Returns the value of TEST1 - 1.
      AMSUB1 = TEST1 - 1.0D0
      RETURN
      END
