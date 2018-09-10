!
!        Copyright (C) 2000-2018 the LUMEN team
!               http://www.attaccalite.com/lumen/
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
subroutine Build_E_matrix(en,Xk,ikbz,V_bands,E_minus,E_plus)
 !
 ! Build energy-overlaps matrices
 !
 use pars,           ONLY:SP,cZERO,cONE
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:k_map,bz_samp
 use vec_operate,    ONLY:k_periodic_idx
 use X_m,            ONLY:DIP_S
 use wrapper_omp,    ONLY:M_by_M_omp
 use matrix_operate, ONLY:hermitian
 use QP_m,           ONLY:QP_nk
 use units,          ONLY:HA2EV
 use electric,       ONLY:l_force_SndOrd
 use nl_optics,      ONLY:NL_bands
 !
 implicit none
 !
 integer,      intent(in) :: ikbz
 type(levels), intent(in) :: en
 type(bz_samp),intent(in) :: Xk
 complex(SP),  intent(in) :: V_bands(NL_bands(2),en%nbf,QP_nk,n_sp_pol)
 complex(SP), intent(out) :: E_minus(en%nbf,en%nbf,3)
 complex(SP), intent(out) :: E_plus (en%nbf,en%nbf,3)
 !
 ! Work Space
 !
 integer     :: i2,idx(3),id,ikp,ikm,ib
 integer     :: ikm_ibz,ikp_ibz,ik_ibz
 complex(SP) :: V_tmp(NL_bands(2),en%nbf),V_dot_E(NL_bands(2),en%nbf)
 integer     :: i_sp
 !
 i_sp=1 ! only for unpolarized systems
 !
 E_minus=cZERO
 E_plus =cZERO
 !
 do id=1,3
   !
   ik_ibz=Xk%sstar(ikbz,1)
   !
   if(k_map%max_kdir(id)/=1) then ! periodic directions
     !
     do ib=1,NL_bands(2)
       V_dot_E(ib,:)=V_bands(ib,:,ik_ibz,i_sp)*en%E(ib,ik_ibz,i_sp)
     enddo
     !
     ! Matrix E(k, k-dk)
     !
     idx=k_map%k_map_inv(ikbz,:)
     idx(id)=idx(id)-1
     idx=k_periodic_idx(idx,k_map)
     ikm=k_map%k_map_dir(idx(1),idx(2),idx(3))
     !
     ! E_minus(l,m; k, k - delta) = \sum_{i,j} V*(l,i; k) e_{k, i} S*(i,j; k-delta, k) V(m,j ;k - delta)
     !
     ikm_ibz=Xk%sstar(ikm,1)
     !
     call M_by_M_omp('C','N',NL_bands(2),en%nbf,NL_bands(2),cONE,DIP_S(:,:,id,ikm,i_sp),&
&                         NL_bands(2),V_bands(:,:,ikm_ibz,i_sp),NL_bands(2),cZERO,V_tmp,NL_bands(2))
     !
     call M_by_M_omp('C','N',en%nbf,en%nbf,NL_bands(2),cONE,V_dot_E,&
&                    NL_bands(2),V_tmp,NL_bands(2),cZERO,E_minus(:,:,id),en%nbf)
     !
     !
     idx=k_map%k_map_inv(ikbz,:)
     idx(id)=idx(id)+1
     idx=k_periodic_idx(idx,k_map)
     ikp=k_map%k_map_dir(idx(1),idx(2),idx(3))  
     !
     ! E_plus(l,m; k, k + delta) = \sum_{i,j} V*(l,i; k) e_{k, i} S(i,j; k - delta, k) V(m,j ;k + delta)
     !
     ikp_ibz=Xk%sstar(ikp,1)
     !
     call M_by_M_omp('N','N',NL_bands(2),en%nbf,NL_bands(2),cONE,DIP_S(:,:,id,ikbz,i_sp),&
&                NL_bands(2),V_bands(:,:,ikp_ibz,i_sp),NL_bands(2),cZERO,V_tmp(:,:),NL_bands(2))
     !
     call M_by_M_omp('C','N',en%nbf,en%nbf,NL_bands(2),cONE,V_dot_E,&
&                    NL_bands(2),V_tmp,NL_bands(2),cZERO,E_plus(:,:,id),en%nbf)
     !
   endif
   !
 enddo
 !
end subroutine Build_E_matrix
