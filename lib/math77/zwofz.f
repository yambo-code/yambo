      subroutine ZWOFZ (Z, W, FLAG)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>>2002-11-06 ZWOFZ Krogh Corrected comments.
c>>1996-03-30 ZWOFZ Krogh Added external stmt., removed INT in type st.
c>>1992-10-13 ZWOFZ WVS Improve efficiency and avoid underflow.
c>>1992-03-13 ZWOFZ FTK Removed implicit statements.
c>>1991-08-23 ZWOFZ WV Snyder Initial adaptation to Math77
c
c     Algorithm 680, collected algorithms from ACM.
c     Reference - GPM Poppe, CMJ Wijers: More efficient computation of
c     the complex error-function, ACM Trans. Math. Software.
c     Vol. 16, No. 1, Pp. 47.
c
c     Modified by W. V. Snyder for inclusion in Math77:
c     Reorganize checking for overflow and loss of precision so
c     there are no redundant or unnecessary checks.  In the process,
c     the range of applicability is expanded to the entire upper
c     half-plane.
c     Reorganize some calculations to be immune from overflow.
c     Split loop for two outer regions into two loops -- faster in
c     region.
c     Use D1MACH to fetch machine characteristics.
c     Use Math77 error message processor.
c
c  Given a complex number z = (xi,yi), this subroutine computes
c  the value of the Faddeeva function w(z) = exp(-z**2)*erfc(-i*z),
c  where erfc is the complex complementary error function and i
c  means sqrt(-1).
c  The accuracy of the algorithm for z in the 1st and 2nd quadrant
c  is 14 significant digits; in the 3rd and 4th it is 13 significant
c  digits outside a circular region with radius 0.126 around a zero
c  of the function.
c
c
c  Argument list
c     Z [in]    = real and imaginary parts of z in Z(1) and Z(2)
c     W [out]   = real and imaginary parts of w(z) in W(1) and W(2)
c     FLAG [out] = an error flag indicating the status of the
c       computation.  Type INTEGER, with values having the following
c       meaning:
c         0 : No error condition,
c        -1 : Overflow would occur,
c        +1 : There would be no significant digits in the answer.
c
c
c  The routine is not underflow-protected but any variable can be
c  put to zero upon underflow.
c
c--   S version uses CWOFZ, R1MACH, r1mach, serv1
c--   D version uses ZWOFZ, D1MACH, d1mach, derv1
c
c
c     RSQPD2 = 2/sqrt(pi) = reciprocal ( sqrt(pi) / 2 ).
c     LN2    = ln(2).
c     SQLGOV = sqrt(ln(RMAX)), where RMAX = the overflow limit for
c              floating point arithmetic.
c     LGOV2  = ln(RMAX) - ln(2)
c     LGUND  = ln(RMIN), where RMIN = underflow limit.
c     MXGONI = the largest possible argument of sin or cos, restricted
c              here to sqrt ( pi / (2*round-off-limit) ).
c     INOGXM = 1 / MXGONI.
c  The reason these values are needed as defined
c  will be explained by comments in the code.
c
      external D1MACH
      logical A
      integer FLAG, I, J, KAPN, N, NU
      double precision C, H, H2, LN2, QLAMDA, QRHO, D1MACH, RSQPD2, RX
      double precision RY, SX, SY, TX, TY, U, U1, U2, UV, V, V1, V2
      double precision W(2), W1, X, XABS, XQUAD, XSUM, Y, YABS, YQUAD
      double precision YSUM, Z(2)
      parameter (RSQPD2 =  1.128379167095512573896158903121545171688d0)
      parameter (LN2    = 0.6931471805599453094172321214581765680755d0)
      double precision INOGXM, LGOV2, LGUND, MXGONI, SQLGOV
      save INOGXM, LGOV2, LGUND, MXGONI, SQLGOV
      data LGOV2 /-1.0d0/
c
      if (lgov2 .le. 0.0d0) then
        lgov2 = log(d1mach(2))
        sqlgov = sqrt(lgov2)
        lgov2 = lgov2 - ln2
        lgund = log(d1mach(1))
        mxgoni = sqrt(8.0d0 / d1mach(4)) / rsqpd2
        inogxm = 1.0d0 / mxgoni
      end if
c
      xabs = abs(z(1))
      yabs = abs(z(2))
      x    = xabs/6.3d0
      y    = yabs/4.4d0
c
      if (x .gt. y) then
        qrho = x * sqrt(1.0d0 + (y/x)**2)
      else if (y .eq. 0.0d0) then
        qrho = 0.0d0
      else
        qrho = y * sqrt(1.0d0 + (x/y)**2)
      end if
c
      a = qrho .lt. 0.292d0
      if (a) then
c
c       qrho .lt. 0.292, equivalently qrho**2 .lt. 0.085264: the Fadeeva
c       function is evaluated using a power-series (Abramowitz and
c       Stegun, equation (7.1.5), p.297).
c       N is the minimum number of terms needed to obtain the required
c       accuracy.
c
c       We know xquad and exp(-xquad) and yqyad and sin(yquad) won't
c       cause any trouble here, because qrho .lt. 1.
        xquad = (xabs - yabs) * (xabs + yabs)
        yquad = 2.0d0*xabs*yabs
        n     = int(6.5d0 + 72.0d0 * (1.0d0-0.85d0 * y) * qrho)
        j     = 2*n+1
        xsum  = rsqpd2/j
        ysum  = 0.0d0
        do 10 i = n, 1, -1
          j    = j - 2
          w1   = (xsum*xquad - ysum*yquad)/i
          ysum = (xsum*yquad + ysum*xquad)/i
          xsum = w1 + rsqpd2/j
 10     continue
        u1 = 1.0d0 - (xsum*yabs + ysum*xabs)
        v1 =         (xsum*xabs - ysum*yabs)
        w1 =  exp(-xquad)
        u2 =  w1*cos(yquad)
        v2 = -w1*sin(yquad)
c
        u  = u1*u2 - v1*v2
        v  = u1*v2 + v1*u2
      else
c
        rx = 0.0d0
        ry = 0.0d0
        sx = 0.0d0
        sy = 0.0d0
c
c       The loops in both branches of the IF block below are similar.
c       They could be combined to reduce space, but extra tests and
c       unnecessary computation would be needed.
c
        if (qrho .lt. 1.0d0) then
c         0.292 .le. qrho .lt. 1.0: w(z) is evaluated by a truncated
c         Taylor expansion, where the Laplace continued fraction
c         is used to calculate the derivatives of w(z).
c         KAPN is the minimum number of terms in the Taylor expansion
c         needed to obtain the required accuracy.
c         NU is the minimum number of terms of the continued fraction
c         needed to calculate the derivatives with the required
c         accuracy.
c         x*x + y*y is more accurate than qrho*qrho here:
          c    = (1.0d0-y) * sqrt(1.0d0-x*x-y*y)
          h    = 1.88d0 * c
          h2   = 2.0d0 * h
          nu   = int(17.5d0 + 26.0d0*c)
          kapn = int(8.5d0  + 34.0d0*c)
c         Select kapn so qlamda doesn't underflow.  Small kapn is good
c         (when possible) for performance also.
          if (h2 .lt. 0.25d0) kapn=min(kapn,1+int(lgund/log(h2)))
          qlamda = h2**(kapn-1)
c         0 < qlamda < 3.76**41 < 3.85d23.
          do 20 n = nu, kapn+1, -1
            tx  = yabs + h + n*rx
            ty  = xabs - n*ry
c           No overflow because tx*rx + ty*ry = 1 and 0.292 < qrho < 1:
            c = 0.5d0/(tx*tx + ty*ty)
            rx = tx*c
            ry = ty*c
20        continue
          do 30 n = kapn, 1, -1
            tx  = yabs + h + n*rx
            ty  = xabs - n*ry
c           No overflow because tx*rx + ty*ry = 1 and 0.292 < qrho < 1:
            c = 0.5d0/(tx*tx + ty*ty)
            rx = tx*c
            ry = ty*c
            tx = qlamda + sx
            sx = rx*tx - ry*sy
            sy = ry*tx + rx*sy
            qlamda = qlamda/h2
30        continue
          u = rsqpd2*sx
          v = rsqpd2*sy
        else
c         qrho .ge. 1.O: w(z) is evaluated using the Laplace continued
c         fraction.
c         NU is the minimum number of terms needed to obtain the
c         required accuracy.
          nu   = int(4.5d0 + (1442.0d0 / (26.0d0 * qrho + 77.0d0)))
          do 40 n = nu, 1, -1
            tx  = yabs + n*rx
            ty  = xabs - n*ry
            if (tx .gt. abs(ty)) go to 50
c           rx = 0.5*tx/(tx**2+ty**2) and ry = 0.5*ty/(tx**2+ty**2),
c           computed without overflow.  Underflow is OK.
            c = tx / ty
            ry = 0.5d0 / (ty * (1.0d0 + c*c))
            rx = ry * c
40        continue
          go to 60
c         Once tx>abs(ty), it stays that way.
50        continue
c           rx = 0.5*tx/(tx**2+ty**2) and ry = 0.5*ty/(tx**2+ty**2),
c           computed without overflow.  Underflow is OK.
            c = ty / tx
            rx = 0.5d0 / (tx * (1.0d0 + c*c))
            ry = rx * c
            n = n - 1
            if (n.eq.0) go to 60
            tx  = yabs + n*rx
            ty  = xabs - n*ry
          go to 50
60        u = rsqpd2*rx
          v = rsqpd2*ry
        end if
c
        if (yabs .eq. 0.0d0) then
          if (xabs .gt. sqlgov) then
            u = 0.0d0
          else
            u = exp(-xabs**2)
          end if
        end if
c
      end if
c
c     Evaluation of w(z) in the other quadrants.
c
      if (z(2) .lt. 0.0d0) then
        if (a) then
          u2 = u2 + u2
          v2 = v2 + v2
        else
c         Check whether sin(2*xabs*yabs) has any precision, without
c         allowing 2*xabs*yabs to overflow.
          if (yabs .gt. xabs) then
            if (yabs .gt. inogxm) then
c             The following protects 2*exp(-z**2) against overflow.
              if (lgov2/yabs .lt. yabs - xabs*(xabs/yabs)) go to 100
              w1 =  2.0d0*exp((yabs-xabs)*(yabs+xabs))
              uv = min(abs(u),abs(v))
              if (w1 .gt. uv) then
                if (xabs .gt. mxgoni/yabs) go to 110
              else
c               We put xabs*(w1/uv) here instead of simply xabs because
c               loss of precision in sin and cos will be diminished
c               relative to uv by w1.
                if (xabs*(w1/uv) .gt. mxgoni/yabs) go to 110
              end if
            end if
          else if (xabs .gt. inogxm) then
            if (lgov2/xabs .lt. xabs - yabs*(yabs/xabs)) then
c             (yabs-xabs)*(yabs+xabs) might have overflowed, but in that
c             case, exp((yabs-xabs)*(yabs+xabs)) would underflow.
              u2 = 0.0d0
              v2 = 0.0d0
              go to 80
            end if
c           (yabs-xabs)*(yabs+xabs) can't overflow here.
            w1 =  2.0d0*exp((yabs-xabs)*(yabs+xabs))
            uv = min(abs(u),abs(v))
            if (w1 .gt. uv) then
              if (yabs .gt. mxgoni/xabs) go to 110
            else
c             We put yabs*(w1/uv) here instead of simply yabs because
c             loss of precision in sin and cos will be diminished
c             relative to uv by w1.
              if (yabs*(w1/uv) .gt. mxgoni/xabs) go to 110
            end if
          end if
          yquad = 2.0d0*xabs*yabs
          u2 =  w1*cos(yquad)
          v2 = -w1*sin(yquad)
80        continue
        end if
c
        u = u2 - u
        v = v2 - v
        if (z(1) .gt. 0.0d0) v = -v
      else
        if (z(1) .lt. 0.0d0) v = -v
      end if
c
      flag = 0
      w(1) = u
      w(2) = v
      return
c
c     Overflow
100   flag = -1
      call ermsg ('ZWOFZ',-1,2,
     1'EXP(-REAL(Z**2)) would overflow, Y .lt. - SQRT(X**2 + LN(infinity
     2/2))'
     2,',')
      call derv1 ('LN(infinity/2)',lgov2,',')
      go to 120
c     No significant digits
110   flag = +1
      call ermsg ('ZWOFZ',+1,2,
     1'Too few significant digits in COS(IMAG(Z**2)),',',')
      call ermor ('2*Y*ABS(X) .lt. -SQRT(2*pi/eps)',',')
      call derv1 ('SQRT(2*pi/eps)',mxgoni,',')
120   call derv1 ('REAL(Z)',z(1),',')
      call derv1 ('IMAG(Z)',z(2),'.')
      w(1) = d1mach(2)
      w(2) = w(1)
      return
c
      end
