!
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG CA
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
subroutine Build_overlaps_det_NEQ(en,Xk,i_sp,ikbz,V_bands,Sm1_plus,Sm1_minus,S_det)
 !
 ! Build overlaps determinants between valence bands
 !
 use pars,           ONLY:SP,cZERO,cONE
 use electrons,      ONLY:levels,n_sp_pol
 use nl_optics,      ONLY:NL_bands
 use R_lattice,      ONLY:k_map,bz_samp
 use vec_operate,    ONLY:k_periodic_idx
 use X_m,            ONLY:DIP_S
 use wrapper    ,    ONLY:M_by_V
 use matrix_operate, ONLY:hermitian
 use QP_m,           ONLY:QP_nk
 use electric,       ONLY:l_force_SndOrd
 !
 implicit none
 !
 integer,      intent(in) :: ikbz
 integer,      intent(in) :: i_sp
 type(levels), intent(in) :: en
 type(bz_samp),intent(in) :: Xk
 complex(SP),  intent(in) :: V_bands(NL_bands(2),en%nbf,QP_nk,n_sp_pol)
 complex(SP), intent(out) :: S_det(6)
 complex(SP), intent(out) :: Sm1_plus (en%nbf,en%nbf,6)
 complex(SP), intent(out) :: Sm1_minus(en%nbf,en%nbf,6)
 !
 ! Work Space
 !
 integer     :: i2,idx(3),id,ikp,ikm
 integer     :: ikm_ibz,ikp_ibz,ik_ibz,max_step,istep
 complex(SP) :: V_tmp(NL_bands(2)),tmp_det
 !
 S_det    =cZERO
 Sm1_plus =cZERO
 Sm1_minus=cZERO
 !
 do id=1,3
   !
   ik_ibz=Xk%sstar(ikbz,1)
   !
   if(k_map%max_kdir(id)/=1) then ! periodic directions
     !
     max_step=1
     if(k_map%max_kdir(id)>=6.and..not.l_force_SndOrd) max_step=2
     !
     do istep=1,max_step
       !
       ! Overlap S(k, k-dk)
       !
       idx=k_map%k_map_inv(ikbz,:)
       idx(id)=idx(id)-istep
       idx=k_periodic_idx(idx,k_map)
       ikm=k_map%k_map_dir(idx(1),idx(2),idx(3))
       !
       ! Sm1_minus(l,m; k, k - delta) = \sum_{i,j} V*(l,i; k) S*(i,j; k-delta, k) V(m,j ;k - delta)
       !
       ikm_ibz=Xk%sstar(ikm,1)
       !
!$omp parallel do default(shared), private(i2,V_tmp)
       do i2=1,en%nbf
         !
         call M_by_V('C',NL_bands(2),NL_bands(2),cONE,DIP_S(:,:,id+(istep-1)*3,ikm,i_sp),&
&                        NL_bands(2),V_bands(:,i2,ikm_ibz,i_sp),1,cZERO,V_tmp,1)
         !
         call M_by_V('C',NL_bands(2),en%nbf,cONE,V_bands(:,:,ik_ibz,i_sp),&
&                        NL_bands(2),V_tmp,1,cZERO,Sm1_minus(:,i2,id+(istep-1)*3),1)
       enddo
!$omp end parallel do
       !
       call SERIAL_inversion(en%nbf,Sm1_minus(:,:,id+(istep-1)*3),tmp_det,.false.)
       !
       idx=k_map%k_map_inv(ikbz,:)
       idx(id)=idx(id)+istep
       idx=k_periodic_idx(idx,k_map)
       ikp=k_map%k_map_dir(idx(1),idx(2),idx(3))  
       !
       ! Sm1_plus(l,m; k, k + delta) = \sum_{i,j} V*(l,i; k) S(i,j; k-delta, k) V(m,j ;k + delta)
       !
       ikp_ibz=Xk%sstar(ikp,1)
       !
!$omp parallel do default(shared), private(i2,V_tmp)
       do i2=1,en%nbf
         call M_by_V('N',NL_bands(2),NL_bands(2),cONE,DIP_S(:,:,id+(istep-1)*3,ikbz,i_sp),&
&                        NL_bands(2),V_bands(:,i2,ikp_ibz,i_sp),1,cZERO,V_tmp,1)
         !
         call M_by_V('C',NL_bands(2),en%nbf,cONE,V_bands(:,:,ik_ibz,i_sp),&
&                      NL_bands(2),V_tmp,1,cZERO,Sm1_plus(:,i2,id+(istep-1)*3),1)
       enddo
!$omp end parallel do
       !
       call SERIAL_inversion(en%nbf,Sm1_plus(:,:,id+(istep-1)*3),S_det(id+(istep-1)*3),.true.)
       !
     enddo  
     !
   endif
   !
 enddo
 !
end subroutine Build_overlaps_det_NEQ
