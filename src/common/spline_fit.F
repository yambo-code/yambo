!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine spline_fit(Ni,No,Xi,Yi,Xo,Yo)
 !
 ! Spline are designed for interpolation,
 ! not for estrapolation.
 !
 use pars, ONLY:SP
 !
 implicit none
 !
 integer, intent(in)    :: Ni,No
 real(SP),intent(in)    :: Xi(Ni),Yi(Ni)
 real(SP),intent(out)   :: Xo(No),Yo(No)
 ! 
 ! Work Space
 !
 integer, parameter :: NDEG=3
 !
 integer  :: NEST,NKNOTS,NWRK
 real(SP) :: W(Ni),IERR,W_SQR
 !
 integer  :: IWRK(Ni+2*NDEG)  
 real(SP) :: KNOTS_ON_SPLINES(Ni+2*NDEG),COEFF_ON_B_SPLINE(Ni+2*NDEG), &
&            WRK(Ni*(NDEG+1)+(Ni+2*NDEG)*(8+5*NDEG))
 !
 W(:)=1._SP
 NEST=Ni+2*NDEG
 NWRK=Ni*(NDEG+1)+(Ni+2*NDEG)*(8+5*NDEG)
 !
 if (Ni<=1) return
 !
 ! Evaluate the splines coeff (COEFF_ON_B_SPLINE)
 ! ===============================================
 call percur(0,Ni,Xi,Yi,W,NDEG,0,NEST,NKNOTS,KNOTS_ON_SPLINES,COEFF_ON_B_SPLINE,W_SQR,WRK,NWRK,IWRK,IERR)
 !
 ! Using the splines coeff (COEFF_ON_B_SPLINE) evaluate Yo on Xo grid
 ! ===================================================================
 call splev(KNOTS_ON_SPLINES,NKNOTS,COEFF_ON_B_SPLINE,NDEG,Xo,Yo,No,IERR)
 !
end subroutine
