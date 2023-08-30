!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
function RIntegrate(F,W,N)
 !
 use pars,    ONLY:SP
 implicit none
 integer,     intent(in)  :: N
 real(SP),    intent(in)  :: W(N),F(N)
 real(SP)                 :: RIntegrate
 !
 ! Work Space
 !
 integer      :: i_w
 real(SP)     :: M,Q
 !
 RIntegrate=0._SP
 !
 do i_w=1,N-1
   M=(F(i_w)-F(i_w+1))/(W(i_w)-W(i_w+1))
   Q=F(i_w)-M*W(i_w)
   RIntegrate=RIntegrate+1._SP/2._SP*M*(W(i_w+1)**2-W(i_w)**2)+Q*(W(i_w+1)-W(i_w))
 enddo
 !
end function
