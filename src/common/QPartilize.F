!
! License-Identifier: GPL
!
! Copyright (C) 2010 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine QPartilize(N,G,W,E,Z,dG_step)
 !
 use pars,    ONLY:SP
 implicit none
 integer,     intent(in)   :: N
 real(SP),    intent(inout):: dG_step
 real(SP),    intent(in)   :: W(N)
 complex(SP), intent(in)   :: G(N)
 complex(SP), intent(out)  :: E,Z
 !
 ! Work Space
 !
 integer          ::i_w,n_poles,dG_steps,ip_larger_R,ip
 integer,parameter::n_max_poles=100
 real(SP)         ::R(n_max_poles),Eqp(n_max_poles),Width(n_max_poles)
 complex(SP)      ::beta(n_max_poles),G_m1(N),A(N)
 !
 G_m1(:)=1./G(:)
 A(:)   =W(:)-G_m1(:)
 !
 if (dG_step>0.) dG_steps=int(dG_step/real(W(2)-W(1))) 
 !
 n_poles=0
 do i_w=1,N-1
   if (real(G_m1(i_w))*real(G_m1(i_w+1))<0.) then
     n_poles=n_poles+1
     Eqp(n_poles)   =real(W(i_w))
     Width(n_poles) =aimag(A(i_w))
     R(n_poles)     =abs(aimag(G(i_w)))
     beta(n_poles)=(A(min(i_w+dG_steps,N))-A(i_w))/real(W(min(i_w+dG_steps,N))-W(i_w))
   endif
   if (n_poles==n_max_poles) exit
 enddo
 ip_larger_R=1
 if (n_poles>1) then
   do ip=2,n_poles
     if (R(ip)>R(ip_larger_R)) ip_larger_R=ip
   enddo
 endif
 !
 Z=1./(1.-beta(ip_larger_R))
 E=Eqp(ip_larger_R)+Z*CMPLX(0.,Width(ip_larger_R))
 !
end subroutine
