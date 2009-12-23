!
! Copyright (C) 2000-2009 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_current(xK,Xen,G_lesser,RT_time)
 !
 ! Evalute current including also the diamagnetic contribution
 !
 use pars,           ONLY:SP,DP,pi
 use units,          ONLY:SPEED_OF_LIGHT
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 use D_lattice,      ONLY:DL_sop,sop_inv
 use SC,             ONLY:SC_bands
 use electrons,      ONLY:nel,levels
 use X_m,            ONLY:DIP_iR_or_P
 use real_time,      ONLY:l_RT_no_diamagnetic,i_time,Efield,n_ext_fields,RT_step, &
&                         l_NE_with_fields,j_current,A_vecpot,l_RT_induced_field
 !
 implicit none
 type(bz_samp), intent(in) ::Xk
 type(levels),  intent(in) ::Xen
 real(SP),      intent(in) ::RT_time
 complex(SP),    intent(in)  :: G_lesser(SC_bands,SC_bands,Xk%nibz)
 !
 ! Work Space
 !
 integer       :: i1,i2,i3,ik,is,SC_bands2
 complex(SP)      :: j_dir(3),j_dir_rot(3),G_tmp(SC_bands,SC_bands)
#if defined _DOUBLE
 complex(DP)   :: zdotc
#else
 complex(SP)   :: cdotc
#endif
 !
 SC_bands2=SC_bands**2
 !
 ! j(r,t) = \sum_ijk e/(2m) \psi^*_i(k) p \psi_j(k) - e^2/(cm) n(r) A(r,t)
 !
 ! j_current(t) = int dr j(r,t)
 !
 j_current(:)=(0._SP,0._SP)
 !
 if(.not.l_NE_with_fields.or.i_time<0) return
 !
! write(100,'(4f16.8)') real(G_lesser(4,5,1)),aimag(G_lesser(4,5,1)),real(G_lesser(5,4,1)),aimag(G_lesser(5,4,1))
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
     DIP_iR_or_P=(0._SP,1._SP)*DIP_iR_or_P
     j_dir(i2)=-real(cdotc(SC_bands2,DIP_iR_or_P(i2,:,:,ik,1),1,G_lesser(:,:,ik),1))
     DIP_iR_or_P=(0._SP,-1._SP)*DIP_iR_or_P
#endif
     !
   enddo
   !
   ! Time-reversal is always excluded in the symmetries list!
   !
   j_dir_rot  =  matmul( dl_sop(:,:,is), j_dir )
   !
   j_current(:)=j_current(:)+j_dir_rot(:)
   !
 enddo
 !
 j_current(:)=j_current(:)/real(Xk%nbz,SP)
 !
 ! Diamagnetic term
 !
 if(.not.l_RT_no_diamagnetic) then
   !
   j_current(:)=j_current(:)+A_vecpot%vecpot(:)*real(nel,SP)/SPEED_OF_LIGHT
   !
 endif
 !
end subroutine RT_current
