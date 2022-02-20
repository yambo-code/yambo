!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG IA
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
subroutine FL_build_tilde_V_bands(FL_V_bands,en,Xk,ikbz,i_sp,istep,id_red,FL_V_tilde)
 !
 ! Build the \tilde Valence bands as defined in Eq. 53 of
 ! PRB 69, 085106 (2004), in terms of Kohn-Sham orbitals
 !
 use pars,           ONLY:SP,cZERO,cONE
 use electrons,      ONLY:levels,n_sp_pol
 use QP_m,           ONLY:QP_nk
 use hamiltonian,    ONLY:H_ref_bands,H_ref_nbands
 use R_lattice,      ONLY:bz_samp,k_map
 use vec_operate,    ONLY:k_periodic_idx
 use DIPOLES,        ONLY:DIP_S
 use wrapper_omp,    ONLY:M_by_M_omp
 use fl_optics,      ONLY:n_tot_fl_modes,max_fl_mode,FL_space_dim,FL_states,n_tot_duffin_modes,&
&                         FL_S_plus,FL_S_minus,max_duffin_mode,max_extra_mode,FL_S_inv_mode
 !
 implicit none
 !
 type(levels), intent(in)  :: en
 type(bz_samp),intent(in)  :: Xk
 integer,      intent(in)  :: ikbz
 integer,      intent(in)  :: i_sp
 integer,      intent(in)  :: istep
 integer,      intent(in)  :: id_red
 complex(SP),  intent(in)  :: FL_V_bands(H_ref_bands(2),n_tot_fl_modes,en%nbf,QP_nk,n_sp_pol)
 complex(SP),  intent(out) :: FL_V_tilde(H_ref_bands(2),n_tot_fl_modes,n_tot_duffin_modes,en%nbf,2) !2 for plus and minus
 !
 ! Work Space
 !
 integer     :: i2,iv1,im,idx(3),id,ikp,ikm
 integer     :: ikm_ibz,ikp_ibz
 complex(SP) :: tmp_Vp(H_ref_bands(2),en%nbf,n_tot_fl_modes),tmp_Vm(H_ref_bands(2),en%nbf,n_tot_fl_modes)
 integer     :: i1_fl,i2_fl,i3_fl,i4_fl,i_m,i_comb_fl,i_shift_comb_fl,i1_fks,i2_fks
 complex(SP) :: FL_Sm1_minus(en%nbf,en%nbf,n_tot_duffin_modes)
 complex(SP) :: FL_Sm1_plus(en%nbf,en%nbf,n_tot_duffin_modes)
 complex(SP) :: dummy_det
 !
 FL_V_tilde=cZERO
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
 FL_Sm1_plus = cZERO
 FL_Sm1_minus = cZERO
 !
 select case(FL_S_inv_mode)
   !
   case("D") !Duffin approach
     !
     call FL_Duffin(en,Xk,FL_Sm1_plus,FL_Sm1_minus,id_red,istep,i_sp,ikbz)
     !
   case("N") !Numerical approach
     !
     !
   case("P") !Perturbative approach
     !
     call error('   [FL] Perturbative inversion of S not yet implemented')
     !
   case("0") !No time dependence - only S^0
     !
     FL_Sm1_plus(:,:,max_duffin_mode+1)  = FL_S_plus(:,:,max_extra_mode+1,id_red+(istep-1)*3,ikbz,i_sp)
     FL_Sm1_minus(:,:,max_duffin_mode+1) = FL_S_minus(:,:,max_extra_mode+1,id_red+(istep-1)*3,ikbz,i_sp)
     !
     call SERIAL_inversion(en%nbf,FL_Sm1_plus(:,:,max_duffin_mode+1),dummy_det,.false.)
     call SERIAL_inversion(en%nbf,FL_Sm1_minus(:,:,max_duffin_mode+1),dummy_det,.false.)
     !
 end select
 ! 
 ! 
 ! 
 do i1_fl=1,n_tot_fl_modes !gamma1
   !
   call M_by_M_omp('N','N',H_ref_nbands,en%nbf,H_ref_bands(2),cONE,  &
  &  DIP_S(H_ref_bands(1):,:,id_red+(istep-1)*3,ikbz,i_sp),H_ref_nbands, &
  &  FL_V_bands(:,i1_fl,:,ikp_ibz,i_sp),H_ref_bands(2),cZERO,tmp_Vp(H_ref_bands(1):,:,i1_fl),H_ref_nbands)
   !
   ! aim137: las dos instancias de H_ref_bands(1): son "j" en mis ecuaciones. o sea, la dim del H.
   ! aim137: pero recordar que al final el H va de H_ref_bands(1) a H_ref_bands(2)
   !
   call M_by_M_omp('C','N',H_ref_nbands,en%nbf,H_ref_bands(2),cONE,  &
  &  DIP_S(:,H_ref_bands(1):,id_red+(istep-1)*3,ikm ,i_sp),H_ref_bands(2), &
  &  FL_V_bands(:,i1_fl,:,ikm_ibz,i_sp),H_ref_bands(2),cZERO,tmp_Vm(H_ref_bands(1):,:,i1_fl),H_ref_nbands)
   !
 enddo
 !
 ! Build \tilde V(k+delta) - \tilde V(k-delta)
 ! ===========================================
 ! where \tilde V(k+delta,n,:) = \sum_m \tilde S^-1(k,k+delta)_{m,n} * Vp(m,:)
 ! 
!$omp parallel do default(shared), private(i2,iv1)
 do i2=H_ref_bands(1),H_ref_bands(2)
   do iv1=1,en%nbf
     do i1_fl=1,n_tot_fl_modes
       do i2_fl=1,n_tot_duffin_modes
         FL_V_tilde(i2,i1_fl,i2_fl,iv1,1)=sum( FL_Sm1_plus(:,iv1,i2_fl)*tmp_Vp(i2,:,i1_fl)) 
         FL_V_tilde(i2,i1_fl,i2_fl,iv1,2)=sum(FL_Sm1_minus(:,iv1,i2_fl)*tmp_Vm(i2,:,i1_fl))
                                                      !aim137: en ^, como elijo aca una columna y no la otra?
       enddo
     enddo
   enddo
 enddo
!$omp end parallel do
 !
end subroutine FL_build_tilde_V_bands
