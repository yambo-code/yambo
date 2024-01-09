!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
real(SP) function HyperbolicSecant_FT(E_center,Gamma_HS,alpha)
 !
 ! This subrutine returns the frequency dependent Green function
 ! which is the imaginary part of the
 ! Fourier transform of the hyperbolic function
 ! 1/2*theta(t)*e^{-i*E_center}*sech^alpha(Gamma/alpha*t)
 ! evaluated at E_center
 !
 use pars,        ONLY:SP,pi,zero_dfl
 !
 implicit none
 !
 real(SP),intent(in)    :: E_center
 real(SP),intent(in)    :: Gamma_HS
 integer, intent(in)    :: alpha
 !
 real(SP)               :: sech_x,cosech_x,x
 real(SP)               :: F1,F2,prefactor
 !
 if(alpha<=0) call error('Hyperbolic secant FFT exist only for alpha>0')
 if(alpha>10) call error('Hyperbolic secant FFT for alpha>10 not implemented')
 !
 x=real(alpha,SP)*pi*E_center/(2._SP*Gamma_HS)
 !
 if(mod(alpha,2)==1) then
   sech_x=  2._SP/(exp(x)+exp(-x))
   F1=(pi/2._SP)*(1._SP/Gamma_HS)*sech_x
 endif
 !
 if(mod(alpha,2)==0) then
   if(abs(x)>zero_dfl) then
     cosech_x=2._SP/(exp(x)-exp(-x))
     F2=(pi/2._SP)*(4._SP/Gamma_HS**2._SP)*E_center*cosech_x
   else
     F2=           (4._SP/Gamma_HS)/real(alpha,SP) 
   endif
 endif
 !
 !
 if(alpha>2) prefactor=(1._SP+E_center**2._SP/Gamma_HS**2._SP)
 !
 select case(alpha)
   case(1)
     HyperbolicSecant_FT=F1
   case(2)
     HyperbolicSecant_FT=F2
   case(3)
     HyperbolicSecant_FT=(1._SP/2._SP)*prefactor*F1
   case(4)
     HyperbolicSecant_FT=(2._SP/3._SP)*prefactor*F2
   case(5)
     HyperbolicSecant_FT=(3._SP/8._SP)*prefactor**2._SP*F1
   case(6)
     HyperbolicSecant_FT=(8._SP/15._SP)*prefactor**2._SP*F2
   case(7)
     HyperbolicSecant_FT=(15._SP/48._SP)*prefactor**3._SP*F1
   case(8)
     HyperbolicSecant_FT=(48._SP/105._SP)*prefactor**3._SP*F2
   case(9)
     HyperbolicSecant_FT=(105._SP/384._SP)*prefactor**4._SP*F1
   case(10)
     HyperbolicSecant_FT=(384._SP/945._SP)*prefactor**4._SP*F2
     !
 end select
 !
 ! More can be created using the recursion relation
 ! F_{alpha+2}=F_{alpha}*prefactor*(alpha/alpha+1)
 !
 !DEBUG <
 !write(100,*) E_center,HyperbolicSecant_FT,Gamma_HS
 !DEBUG >
 !
end function
