!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
function Laplace_transform(F,N,dT,Eo)
 !
 use pars,    ONLY:SP,cI
 implicit none
 integer,  intent(in)  :: N
 real(SP), intent(in)  :: dT,Eo
 real(SP), intent(in)  :: F(N)
 real(SP)              :: Laplace_transform
 !
 ! Work Space
 !
 integer   :: i_t
 !
 Laplace_transform=0._SP
 !
 if (abs(Eo)>1.E-5) then
   do i_t=1,N-1
     Laplace_transform=Laplace_transform+aimag(F(i_t)*exp(cI*i_t*dT*Eo)/Eo*(exp(cI*dT*Eo)-1))
   enddo
 else
   do i_t=1,N
     Laplace_transform=Laplace_transform+F(i_t)*dT
   enddo
 endif
 !
end function
