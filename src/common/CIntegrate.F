!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
function CIntegrate(F,W,N)
 !
 use pars,    ONLY:SP
 implicit none
 integer,     intent(in)  :: N
 real(SP),    intent(in)  :: W(N)
 complex(SP), intent(in)  :: F(N)
 complex(SP)              :: CIntegrate
 !
 ! Work Space
 !
 integer      :: i_w
 complex(SP)  :: M,Q
 !
 CIntegrate=(0._SP,0._SP)
 !
 do i_w=1,N-1
   M=(F(i_w)-F(i_w+1))/(W(i_w)-W(i_w+1))
   Q=F(i_w)-M*W(i_w)
   CIntegrate=CIntegrate+1./2.*M*(W(i_w+1)**2.-W(i_w)**2.)+Q*(W(i_w+1)-W(i_w))
 enddo
 !
end function
