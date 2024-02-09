!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine Kramers_Kronig(fi,wi,ni,fo,wo,no,y0)
 !
 ! Calculates Kramers-Kronig transformation:
 !
 ! fo(i) = Int_{wi(1)}^{wi(last)} fi(y)/(wo(i)-y-y0) dy
 !
 ! Remember wo MUST HAVE A NON ZERO IMAGINARY PART.
 !
 ! wi must be real
 !
 ! fo is not INITIALIZED to 0. !
 !
 use pars,    ONLY:SP
 !
 implicit none
 !
 integer,     intent(in)    :: ni,no
 real(SP),    intent(in)    :: wi(ni)
 complex(SP), intent(in)    :: fi(ni),wo(no),y0
 complex(SP), intent(inout) :: fo(no)
 !
 ! Work Space
 !
 integer      :: i1,i2
 complex(SP)  :: m(ni-1),q(ni-1)
 !
 do i1=1,ni-1
   m(i1)=(fi(i1)-fi(i1+1))/(wi(i1)-wi(i1+1))
   q(i1)=fi(i1)-m(i1)*wi(i1)
 enddo
 !
 !
 do i1=1,no
   do i2=1,ni-1
     fo(i1)=fo(i1)+m(i2)*(wi(i2)-wi(i2+1))+&
&           (q(i2)+m(i2)*(wo(i1)-y0))*&
&           log((wi(i2)-wo(i1)+y0)/(wi(i2+1)-wo(i1)+y0))
   enddo
 enddo
 !
end subroutine
