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
!                  \
! Evalute current including also the diamagnetic contribution
!
subroutine RT_current(j_current,en,xK,A_t,EF_q0,G_lesser)
 !
 use pars,           ONLY:SP,DP,pi
 use matrix_operate, ONLY:m3det
 use R_lattice,      ONLY:bz_samp,RL_vol,rl_sop
 use D_lattice,      ONLY:nsym,i_time_rev,dl_sop,sop_inv,i_time_rev
 use SC,             ONLY:SC_bands
 use electrons,      ONLY:nel,levels
 use X_m,            ONLY:DIP_iR_or_P
 use com,            ONLY:msg
 !
 implicit none
 type(levels),  intent(in) ::en       
 type(bz_samp), intent(in) ::Xk
 complex(SP), intent(in)  :: G_lesser(en%nbm,en%nbm,Xk%nibz)
 complex(SP), intent(in)  :: A_t
 real(SP),    intent(in)  :: EF_q0(3)
 real(SP),    intent(out) :: j_current(3)
 !
 ! Work Space
 !
 integer       :: i1,i2,ik,is,SC_bands2
 real(SP)      :: j_dir(3),j_dir_rot(3)
 complex(DP)   :: zdotc
 complex(SP)   :: cdotc
 !
 SC_bands2=SC_bands**2
 !
 call msg('nr','[RT] Eps Analyzer')
 !
 ! j(r,t) = \sum_ijk e/2m \psi^*_i(k) p \psi_j(k) - e^2/m n(r) A(r,t)
 !
 ! j_current(t) = int dr j(r,t)
 !
 j_current(:)=0.
 !
 do i1=1,Xk%nbz
   !
   ik = Xk%sstar(i1,1)
   is = sop_inv(Xk%sstar(i1,2))      
   !
   do i2=1,3
     !      
#if defined _DOUBLE
     j_dir(i2)=-aimag(zdotc(SC_bands2,DIP_iR_or_P(i2,:,:,ik,1),1,G_lesser(:,:,ik),1))
#else
     j_dir(i2)=-aimag(cdotc(SC_bands2,DIP_iR_or_P(i2,:,:,ik,1),1,G_lesser(:,:,ik),1))
#endif
     !
   enddo
   !
   ! To be checked again for is > sym/(i_time_rev+1)
   !
   if(is >  nsym/(i_time_rev+1)) then
     j_dir_rot  =   matmul( dl_sop(:,:,is), j_dir )*dl_sop(3,3,is) 
   else
     j_dir_rot  =  -matmul( dl_sop(:,:,is), j_dir )*dl_sop(3,3,is)  
   endif
   !
   j_current(:)=j_current(:)+j_dir_rot(:)
   !
 enddo
 !
 j_current(:)=j_current(:)*RL_vol/(Xk%nbz*(2.*pi)**3)
 !
 ! Diamagnetic term. It is correct??
 !
 j_current(:)=j_current(:)+A_t*EF_q0(:)*nel*RL_vol/(Xk%nbz*(2.*pi)**3) 
 !
 !
end subroutine RT_current
