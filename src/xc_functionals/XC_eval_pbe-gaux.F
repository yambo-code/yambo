subroutine XC_eval_pbegaux(spin_rho_DP,sigma,exsr,vx1,vx2)

use D_lattice,   ONLY:DL_vol
use FFT_m,         ONLY:fft_size
use pars,          ONLY:DP
use electrons,     ONLY:n_spin
use xc_functionals, ONLY:xc_spin


implicit none
real(DP),intent(in) :: spin_rho_DP(n_spin,fft_size),sigma(1,fft_size)
real(DP),intent(out) :: exsr, vx1(n_spin,fft_size), vx2(xc_spin(1),fft_size)
real(DP) :: RHO,GRHO,sxsr,v1xsr, v2xsr
!
!parameter 
real(DP), parameter :: alpha_gau= 0.150_DP
integer  :: ir
!
 exsr=0.0_DP
 do ir=1,fft_size
   RHO=spin_rho_DP(1,ir)
   GRHO=sigma(1,ir)
   call pbexgau(RHO,GRHO,sxsr,v1xsr,v2xsr,alpha_gau)
   exsr=exsr+sxsr
 enddo
 exsr=exsr*real(DL_vol,DP)/real(fft_size,DP)
!Then consider the EXX fraction outside the subs

!
 contains

SUBROUTINE pbexgau(RHO,GRHO,sxsr,v1xsr,v2xsr,alpha_gau)
!-----------------------------------------------------------------------
!
!      use kinds, ONLY : DP

      IMPLICIT REAL*8 (A-H,O-Z)

      PARAMETER(SMALL=1.D-20,SMAL2=1.D-08)
      PARAMETER(US=0.161620459673995492D0,AX=-0.738558766382022406D0, &
                UM=0.2195149727645171D0,UK=0.8040D0,UL=UM/UK)
      REAL(DP), PARAMETER :: f1 = -1.10783814957303361_DP, alpha = 2.0_DP/3.0_DP
!     ==--------------------------------------------------------------==

      RS = RHO**(1.0_DP/3.0_DP)
      VX = (4.0_DP/3.0_DP)*f1*alpha*RS
      AA    = GRHO
      RR    = 1.0_DP/(RHO*RS)
      EX    = AX/RR
! AX is 3/4/PI*(3*PI*PI)**(1/3). This is the same as -c1*c2 in pbex().
      S2    = AA*RR*RR*US*US
      S = SQRT(S2)
      IF(S.GT.10.D0) THEN
        S = 10.D0
      ENDIF
      CALL pbe_gauscheme(RHO,S,alpha_gau,FX,D1X,D2X)
      sxsr = EX*FX        ! - EX
      DSDN = -4.D0/3.D0*S/RHO
      V1Xsr = VX*FX + (DSDN*D2X+D1X)*EX   ! - VX
      DSDG = US*RR
      V2Xsr = EX*1.D0/SQRT(AA)*DSDG*D2X

! NOTE, here sx is the total energy density,
! not just the gradient correction energy density as e.g. in pbex()
! And the same goes for the potentials V1X, V2X

!     ==--------------------------------------------------------------==
      RETURN
      END SUBROUTINE pbexgau

!
!-----------------------------------------------------------------------
      SUBROUTINE pbe_gauscheme(rho,s,alpha_gau,Fx,dFxdr,dFxds)
!--------------------------------------------------------------------

      Implicit None
      Real*8 rho,s,alpha_gau,Fx,dFxdr,dFxds
!     input: charge and squared gradient and alpha_gau
!     output: GGA enhancement factor of gau-PBE
!     output: d(Fx)/d(s) , d(Fx)/d(rho)

      Real*8 Kx, Nx
!     PBE96 GGA enhancement factor
!     GGA enhancement factor of Gaussian Function

      Real*8 bx, cx, PI, sqrtpial, Prefac, term_PBE, Third, KsF
      Real*8 d1sdr, d1Kxds, d1Kxdr, d1bxdr, d1bxds, d1bxdKx, &
           d1Nxdbx,d1Nxdr, d1Nxds

      Real*8, external :: qe_erf,TayExp

      Real*8 Zero,One,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten

      Save Zero,One,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten
      Data Zero,One,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten &
        / 0D0,1D0,2D0,3D0,4D0,5D0,6D0,7D0,8D0,9D0,10D0 /

      Real*8 k , mu
      Data k / 0.804d0 / , mu / 0.21951d0 /
!     parameters of PBE functional

      Third = One/Three
      PI = ACos(-One)
      KsF = (Three*PI*PI*rho)**Third
      sqrtpial = sqrt(PI/alpha_gau)
      Prefac = Two *sqrt(PI/alpha_gau) / Three

!     PBE96 GGA enhancement factor part
      term_PBE = One / (One + s*s*mu/k)
      Kx =  One + k - k * term_PBE

!     GGA enhancement factor of Gaussian Function part
      bx = sqrt(Kx*alpha_gau) / KsF

!      cx = exp(-One/Four/bx/bx) - One
      If(Abs(One/bx/bx) .lt. 1.0D-4) then
         cx = TayExp(-One/bx/bx)
      else
         cx = exp(-One/bx/bx) - One
      endIf

      Nx = bx * Prefac * ( sqrt(PI) * qe_erf(One/bx) + &
       (bx - Two*bx*bx*bx)*cx - Two*bx )

      if(Abs(Nx) .lt. 1.0D-15)then
        Nx = Zero
      else if ((One - Abs(Nx)) .lt. 1.0D-15)then
        Nx = One
      else
        Nx = Nx
      endIf
! for convergency end

      Fx =  Kx * Nx

!     1st derivatives
      d1sdr = - Four / Three * s / rho

      d1Kxds = Two * s * mu * term_PBE * term_PBE
      d1Kxdr = d1Kxds * d1sdr
      d1bxdKx = bx / (Two* Kx)

      d1bxdr = - bx /(Three*rho) + d1Kxdr * d1bxdKx

      d1bxds =  d1bxdKx * d1Kxds

      d1Nxdbx =  Nx/bx - Prefac * bx * Three * &
                  ( cx*(One + Two*bx*bx) + Two )

      d1Nxdr = d1Nxdbx * d1bxdr
      d1Nxds = d1Nxdbx * d1bxds

      dFxdr = d1Kxdr * Nx + Kx * d1Nxdr
      dFxds = d1Kxds * Nx + Kx * d1Nxds

      RETURN
      END SUBROUTINE pbe_gauscheme
!
!      FUNCTION TayExp(X)
!      Real*8 TAYEXP,X
!      INTEGER NTERM,I
!      Real*8 SUMVAL,IVAL,COEF
!      PARAMETER (NTERM=16)
!
!      SUMVAL = X
!      IVAL = X
!      COEF = 1.0D0
!      DO 10 I = 2,NTERM
!         COEF = COEF * I
!         IVAL = IVAL * (X / COEF)
!         SUMVAL = SUMVAL + IVAL
! 10         CONTINUE
!      TAYEXP = SUMVAL
!      RETURN
!      END FUNCTION TayExp
! gau-pbe out
end subroutine XC_eval_pbegaux

      FUNCTION TayExp(X)
      Real*8 TAYEXP,X
      INTEGER NTERM,I
      Real*8 SUMVAL,IVAL,COEF
      PARAMETER (NTERM=16)

      SUMVAL = X
      IVAL = X
      COEF = 1.0D0
      DO 10 I = 2,NTERM
         COEF = COEF * I
         IVAL = IVAL * (X / COEF)
         SUMVAL = SUMVAL + IVAL
 10         CONTINUE
      TAYEXP = SUMVAL
      RETURN
      END FUNCTION TayExp
!
