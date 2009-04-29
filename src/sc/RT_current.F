!
! Copyright (C) 2000-2008 A. Marini, C. Attaccalite and the YAMBO team 
!              http://www.yambo-code.org
! 
! This file is distributed under the terms of the GNU 
! General Public License. You can redistribute it and/or 
! modify it under the terms of the GNU General Public 
! License as published by the Free Software Foundation; 
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will 
! be useful, but WITHOUT ANY WARRANTY; without even the 
! implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License 
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
! Evalute current including also the diamagnetic contribution
!
subroutine RT_current(j_current,en,xK,A_t,EF_q0,G_lesser)
 !
 use pars,          ONLY:SP,DP
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:nsym,i_time_rev
 use SC,            ONLY:SC_bands
 use real_time,     ONLY:Grad
 use electrons,     ONLY:nel,levels
 !
 implicit none
 type(levels),  intent(in) ::en       
 type(bz_samp), intent(in) ::Xk
 complex(SP), intent(in)  :: G_lesser(en%nbm,en%nbm,Xk%nibz)
 complex(SP), intent(in)  :: A_t
 real(SP),    intent(in)  :: EF_q0(3)
 complex(SP), intent(out) :: j_current(3)
 !
 ! Work Space
 !
 integer :: i1,i2,ik,i_spin,SC_bands2
 complex(DP)   :: zdotc
 complex(SP)   :: cdotc
 !
 j_current(:)=(0._SP,0._SP)
 SC_bands2=SC_bands**2
 !
 do ik=1,Xk%nibz
   do i1=1,3
     !      
#if defined _DOUBLE
     j_current(i1)=j_current(i1)-(0._DP,1._DP)*zdotc(SC_bands2,Grad(i1,:,:,ik,1),1,G_lesser(:,:,ik),1)*Xk%weights(ik)
#else
     j_current(i1)=j_current(i1)-(0._SP,1._SP)*cdotc(SC_bands2,Grad(i1,:,:,ik,1),1,G_lesser(:,:,ik),1)*Xk%weights(ik)
#endif
     !
   enddo
 enddo
 !
 ! Diamagnetic term. It is correct??
 !
! j_current(:)=j_current(:)+A_t*EF_q0(:)*nel
 !
 !
end subroutine RT_current
