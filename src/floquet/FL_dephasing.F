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
subroutine FL_dephasing(en,Xk,ik,i_sp_pol,FL_V_bands,H_nl_sc)
 !
 use pars,           ONLY:SP,cI,cZERO,pi,cONE
 use units,          ONLY:SPEED_OF_LIGHT
 use electrons,      ONLY:levels,n_sp_pol
 use electric,       ONLY:l_force_SndOrd
 use QP_m,           ONLY:QP_nk
 use hamiltonian,    ONLY:H_ref_bands
 use R_lattice,      ONLY:bz_samp,k_map
 use D_lattice,      ONLY:a
 use matrix_operate, ONLY:hermitian
 use fields,         ONLY:gauge_field,Efield
 use nl_optics,      ONLY:NL_damping,NL_bands
 use fl_optics,      ONLY:max_fl_mode,FL_space_dim,n_tot_fl_modes,FL_states,&
&                         n_tot_duffin_modes,max_duffin_mode
 !
 implicit none
 !
 type(levels), intent(in)      :: en
 type(bz_samp),intent(in)      :: Xk
 integer,      intent(in)      :: ik
 integer,      intent(in)      :: i_sp_pol
 complex(SP),  intent(in)      :: FL_V_bands(NL_bands(2),n_tot_fl_modes,en%nbf(i_sp_pol))
 complex(SP),  intent(inout)   :: H_nl_sc(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 !
 ! Work Space
 !
 integer :: i1,i2,im,id,id_red
 integer :: ikbz,istep,max_step
 integer :: i1_fl,i2_fl,i3_fl,i4_fl,i_m,i_comb_fl,i_shift_comb_fl,i1_fks,i2_fks
 complex(SP) :: I_deph(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 !
 I_deph=cZERO
 !
 ! A
 !
 do i1=H_ref_bands(1),H_ref_bands(2)        !i
   do i1_fl=1,n_tot_fl_modes                !eta
     i1_fks = i1_fl + (i1-1)*n_tot_fl_modes !i,eta
     !
     do i2=H_ref_bands(1),H_ref_bands(2)        !j
       do i2_fl=1,n_tot_fl_modes                !gamma
         i2_fks = i2_fl + (i2-1)*n_tot_fl_modes !j,gamma
         !
         do i3_fl=1,n_tot_fl_modes
           !
           i_shift_comb_fl = (i3_fl-(max_fl_mode+1)) &
                          &+ (i2_fl-(max_fl_mode+1)) &
                          &- (i1_fl-(max_fl_mode+1))
           !
           if (abs(i_shift_comb_fl) > max_fl_mode) cycle 
           i_comb_fl = i_shift_comb_fl + (max_fl_mode+1)
           !
	   do im=1,en%nbf(i_sp_pol)
             I_deph(i1_fks,i2_fks) = I_deph(i1_fks,i2_fks) + & 
                                   & (conjg(FL_V_bands(i2,i_comb_fl,im)) * &
                                   & FL_V_bands(i1,i3_fl,im))
           enddo
           !
         enddo ! i3_fl
         !
       enddo ! i2_fl
     enddo   ! i2 band
     !
   enddo ! i1_fl
 enddo ! i1 band
 !
 ! B term
 !
 do i1=H_ref_bands(1),en%nbf(i_sp_pol)          ! i (only occ)
   do i1_fl=1,n_tot_fl_modes                    !eta
    i1_fks = i1_fl + (i1-1)*n_tot_fl_modes      !i,eta
    !
     do i2=H_ref_bands(1),H_ref_bands(2)        !j
       do i2_fl=1,n_tot_fl_modes                !gamma
         i2_fks = i2_fl + (i2-1)*n_tot_fl_modes !j,gamma
         !
         i_shift_comb_fl = (i2_fl-(max_fl_mode+1)) &
                        &- (i1_fl-(max_fl_mode+1))
         !
         if (abs(i_shift_comb_fl) > max_fl_mode) cycle 
         i_comb_fl = i_shift_comb_fl + (max_fl_mode+1)
         !
         I_deph(i1_fks,i2_fks) = I_deph(i1_fks,i2_fks) - & 
                               & conjg(FL_V_bands(i2,i_comb_fl,i1))
         !
           enddo ! i1_fl
         enddo ! i1 band
         !
       enddo ! i2_fl
     enddo   ! i2 band

 ! $omp parallel do default(shared), private(i1,i1_fl,i1_fks)
 do i1=H_ref_bands(1),H_ref_bands(2)
   do i1_fl=1,n_tot_fl_modes
     i1_fks = i1_fl + (i1-1)*n_tot_fl_modes
     !
     H_nl_sc(i1_fks,:)=H_nl_sc(i1_fks,:) - &
     &         cI * NL_damping * I_deph(i1_fks,:)
     !
   enddo
 enddo
 ! $omp end parallel do
 !
end subroutine FL_dephasing
