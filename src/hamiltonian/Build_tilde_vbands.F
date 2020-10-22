!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine Build_tilde_vbands(V_bands,en,Xk,ikbz,i_sp,istep,id_red,V_tilde)
 !
 ! Build the \tilde Valence bands as defined in Eq. 53 of
 ! PRB 69, 085106 (2004), in terms of Kohn-Sham orbitals
 !
 use pars,           ONLY:SP,cZERO,cONE
 use electrons,      ONLY:levels,n_sp_pol
 use electric,       ONLY:Sm1_plus,Sm1_minus
 use QP_m,           ONLY:QP_nk
 use nl_optics,      ONLY:NL_bands,NL_nbands
 use R_lattice,      ONLY:bz_samp,k_map
 use vec_operate,    ONLY:k_periodic_idx
 use DIPOLES,        ONLY:DIP_S
 use wrapper_omp,    ONLY:M_by_M_omp
 !
 implicit none
 !
 type(levels), intent(in)      :: en
 type(bz_samp),intent(in)      :: Xk
 complex(SP),  intent(in)      :: V_bands(NL_bands(2),en%nbf,QP_nk,n_sp_pol)
 integer,      intent(in)      :: ikbz
 integer,      intent(in)      :: i_sp
 integer,      intent(in)      :: istep
 integer,      intent(in)      :: id_red
 complex(SP),  intent(out)     :: V_tilde(en%nbf,NL_bands(2),2)
 !
 ! Work Space
 !
 integer :: i2,iv1,im,idx(3),id,ikp,ikm
 integer :: ikm_ibz,ikp_ibz
 complex(SP) :: tmp_Vp(NL_bands(2),en%nbf),tmp_Vm(NL_bands(2),en%nbf)
 !
 V_tilde=cZERO
 !
 if(k_map%max_kdir(id_red)==1) return
 !
 idx=k_map%k_map_inv(ikbz,:)
 idx(id_red)=idx(id_red)-istep
 idx=k_periodic_idx(idx,k_map)
 ikm=k_map%k_map_dir(idx(1),idx(2),idx(3))
 !
 idx=k_map%k_map_inv(ikbz,:)
 idx(id_red)=idx(id_red)+istep
 idx=k_periodic_idx(idx,k_map)
 ikp=k_map%k_map_dir(idx(1),idx(2),idx(3))
 !
 ! Indexes in the IBZ
 ! 
 ikm_ibz=Xk%sstar(ikm,1)
 ikp_ibz=Xk%sstar(ikp,1)
 !
 ! Construct \tilde V_bands(k,k+delta) and \tilde V_bands(k,k-delta)
 !
 ! Build Vp and Vm (stored in tmp_Vp and tmp_Vm)
 !
 ! m,n indexes on valence bands
 !
 ! i,j indexes on all bands CMR basis
 !
 ! Vp(m,i) = \sum_{j} S(k,K+delta)_{i,j}   V(m,j)
 !
 ! Vm(m,i) = \sum_{j} S^H(k-delta,K)_{j,i} V(m,j)
 !
 ! 
 ! Build \tilde V(k+delta) - \tilde V(k-delta)
 ! ===========================================
 ! where \tilde V(k+delta,n,:) = \sum_m \tilde S^-1(k,k+delta)_{m,n} * Vp(m,:)
 ! 
 call M_by_M_omp('N','N',NL_nbands,en%nbf,NL_bands(2),cONE,  &
&  DIP_S(NL_bands(1):,:,id_red+(istep-1)*3,ikbz,i_sp),NL_nbands, &
&  V_bands(:,:,ikp_ibz,i_sp),NL_bands(2),cZERO,tmp_Vp(NL_bands(1):,:),NL_nbands)
 !
 call M_by_M_omp('C','N',NL_nbands,en%nbf,NL_bands(2),cONE,  &
&  DIP_S(:,NL_bands(1):,id_red+(istep-1)*3,ikm ,i_sp),NL_bands(2), &
&  V_bands(:,:,ikm_ibz,i_sp),NL_bands(2),cZERO,tmp_Vm(NL_bands(1):,:),NL_nbands)
 !
 ! Build \tilde V(k+delta) - \tilde V(k-delta)
 ! ===========================================
 ! where \tilde V(k+delta,n,:) = \sum_m \tilde S^-1(k,k+delta)_{m,n} * Vp(m,:)
 ! 
!$omp parallel do default(shared), private(i2,iv1)
 do i2=NL_bands(1),NL_bands(2)
   do iv1=1,en%nbf
     V_tilde(iv1,i2,1)=sum( Sm1_plus(:,iv1,id_red+(istep-1)*3,ikbz,i_sp)*tmp_Vp(i2,:)) 
     V_tilde(iv1,i2,2)=sum(Sm1_minus(:,iv1,id_red+(istep-1)*3,ikbz,i_sp)*tmp_Vm(i2,:))
   enddo
 enddo
!$omp end parallel do
 !
end subroutine Build_tilde_vbands
