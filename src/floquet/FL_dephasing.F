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
subroutine FL_dephasing(FL_V_bands,en,Xk,ik,i_sp,H_nl_sc)
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
 use fl_optics,      ONLY:max_fl_mode,FL_space_dim,n_tot_fl_modes,FL_states,n_tot_duffin_modes,max_duffin_mode
 !
 implicit none
 !
 type(levels), intent(in)      :: en
 type(bz_samp),intent(in)      :: Xk
 integer,      intent(in)      :: ik
 integer,      intent(in)      :: i_sp
 complex(SP),  intent(in)      :: FL_V_bands(NL_bands(2),n_tot_fl_modes,en%nbf,QP_nk,n_sp_pol)
 complex(SP),  intent(inout)   :: H_nl_sc(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 !
 ! Work Space
 !
 integer :: i1,i2,im,id,id_red
 integer :: ikbz,istep,max_step
 complex(SP) :: I_deph(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 complex(SP) :: I_stabilization(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 integer :: i1_fl,i2_fl,i3_fl,i4_fl,i_m,i_comb_fl,i_shift_comb_fl,i1_fks,i2_fks
 logical :: l_stabilization,l_force_Hermitian
 !
 I_deph=cZERO
 I_stabilization=cZERO
 l_stabilization = .false.
 l_force_Hermitian = .false.
 !
 ! off diagonal part
 !
 do i1=H_ref_bands(1),H_ref_bands(2) !i
   do i2=H_ref_bands(1),H_ref_bands(2) !j
     do i1_fl=1,n_tot_fl_modes !eta
       i1_fks = i1_fl + (i1-1)*n_tot_fl_modes !i,eta
       do i2_fl=1,n_tot_fl_modes !gamma
         i2_fks = i2_fl + (i2-1)*n_tot_fl_modes !j,gamma
         !
         !aim137 DEBUG< trato de hacer esto solo en la diagonal
         !if ( i1_fks /= i2_fks ) cycle
         !aim137 DEBUG>
         do i3_fl=1,n_tot_fl_modes
             !
             i_shift_comb_fl = (i3_fl-(max_fl_mode+1)) &
                            &+ (i2_fl-(max_fl_mode+1)) &
                            &- (i1_fl-(max_fl_mode+1))
             !
             if (abs(i_shift_comb_fl) > max_fl_mode) cycle 
             i_comb_fl = i_shift_comb_fl + (max_fl_mode+1)
             !
             do i_m=1,en%nbf 
               I_deph(i1_fks,i2_fks) = I_deph(i1_fks,i2_fks) + & 
                                     & (conjg(FL_V_bands(i2,i_comb_fl,i_m,ik,i_sp)) * &
                                     & FL_V_bands(i1,i3_fl,i_m,ik,i_sp))
             enddo
             !
         enddo ! i3_fl
         !
       enddo ! i2_fl
     enddo ! i1_fl
   enddo ! i2 band
 enddo ! i1 band
 !
 ! diagonal part
 !
 !aim137 DEBUG <
 !print*, '---------------------- kpoint ',ik
 !do i1_fks=FL_states(1),FL_states(2)
 !print*, 'row ',i1_fks 
 !print*, I_deph(i1_fks,:)
 !enddo
 !call error('..')
 !aim137 DEBUG >
 !
 do i1=H_ref_bands(1),en%nbf !i
   do i1_fl=1,n_tot_fl_modes !eta
     !
     i1_fks = i1_fl + (i1-1)*n_tot_fl_modes !i,eta
     !
     I_deph(i1_fks,i1_fks) = I_deph(i1_fks,i1_fks) - 1._SP
     !
   enddo
 enddo
 !
 !aim137 DEBUG <
 !print*, sum(I_deph)
 !call error('..')
 !aim137 DEBUG >
 !
 if (l_stabilization) then
  !do i1=H_ref_bands(1),H_ref_bands(2) !i
  !  do i1_fl=1,n_tot_fl_modes !eta
  !    !
  !    i1_fks = i1_fl + (i1-1)*n_tot_fl_modes !i,eta
  !    !
  !    if (i1 <= en%nbf) I_stabilization(i1_fks,i1_fks) = -0.05_SP
  !    if (i1 >  en%nbf) I_stabilization(i1_fks,i1_fks) = +0.05_SP
  !    !
  !  enddo
  !enddo
   !
!  i1_fks = (4-1)*n_tot_fl_modes + (max_fl_mode+1)
!  I_stabilization(i1_fks,i1_fks) = -100.9_SP
!  i2_fks = (5-1)*n_tot_fl_modes + (max_fl_mode+1) + 1
!  I_stabilization(i2_fks,i2_fks) = +100.9_SP
!  !
   I_deph = I_deph * 0.4 + I_stabilization * 0.6
 endif
 !
 if (l_force_Hermitian) then
   !
   I_deph = I_deph + conjg(I_deph)
   !
 endif
 !aim137 DEBUG <
 !
 ! $omp parallel do default(shared), private(i1,id)
  do i1=H_ref_bands(1),H_ref_bands(2)
    do i1_fl=1,n_tot_fl_modes
      i1_fks = i1_fl + (i1-1)*n_tot_fl_modes
        H_nl_sc(i1_fks,:)=H_nl_sc(i1_fks,:) - &
 &      cI * NL_damping * I_deph(i1_fks,:)
    enddo
  enddo
 ! $omp end parallel do
 !
end subroutine FL_dephasing
