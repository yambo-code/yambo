!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG CA IA
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
subroutine FL_build_W_operator(en,Xk,A_input,ik,i_sp,FL_V_bands,H_nl_sc)
 !
 ! Build the Hermitian W operator eq. 62 and 63
 ! in PRB 69, 085106 (2004)
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
 use fl_optics,      ONLY:max_fl_mode,FL_space_dim,n_tot_fl_modes,FL_states,n_tot_duffin_modes,&
&                         max_duffin_mode,FL_E_field_phase
 !
 implicit none
 !
 type(levels), intent(in)      :: en
 type(bz_samp),intent(in)      :: Xk
 type(gauge_field), intent(in) :: A_input
 integer,      intent(in)      :: ik
 integer,      intent(in)      :: i_sp
 complex(SP),  intent(in)      :: FL_V_bands(H_ref_bands(2),n_tot_fl_modes,en%nbf(i_sp),QP_nk)
 complex(SP),  intent(inout)   :: H_nl_sc(FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 !
 ! Work Space
 !
 integer :: i1,i2,im,id,id_red
 integer :: ikbz,istep,max_step
 complex(SP) :: Wk(3,FL_states(1):FL_states(2),FL_states(1):FL_states(2))
 complex(SP) :: Wop(FL_states(1):FL_states(2),FL_states(1):FL_states(2),2)
 complex(SP) :: FL_V_tilde(H_ref_bands(2),n_tot_fl_modes,n_tot_duffin_modes,en%nbf(i_sp),2) 
 complex(SP) :: E_vec_pot(3)
 integer :: i1_fl,i2_fl,i3_fl,i4_fl,i_m,i_comb_fl,i_shift_comb_fl,i1_fks,i2_fks
 complex(SP) :: ph_p,ph_m
 !
 E_vec_pot=-A_input%vecpot_vel/SPEED_OF_LIGHT
 !
 Wk=cZERO
 ph_p = exp(+cI*pi*FL_E_field_phase)
 ph_m = exp(-cI*pi*FL_E_field_phase)
 !
 ikbz=Xk%k_table(ik,1)
 !
 do id_red=1,3  ! loop on reciprocal lattice
   !
   Wop=cZERO
   !
   if(k_map%max_kdir(id_red)/=1) then
     !
     max_step=1
     !
     ! I need at leat 5 k-points in the line to
     ! use the second order formula
     !
     if(k_map%max_kdir(id_red)>=6.and..not.l_force_SndOrd) max_step=2
     !
     do istep=1,max_step
       !
       call FL_build_tilde_V_bands(en,Xk,ikbz,i_sp,istep,id_red,FL_V_bands,FL_V_tilde)
       ! Build the W operator as
       !
       ! W(i,j) = \sum_m V*(m,i) * [\tilde Vp(m,j) - \tilde Vm(m,j) ]
       !
       do i1=H_ref_bands(1),H_ref_bands(2)
         do i2=H_ref_bands(1),H_ref_bands(2)
           do i1_fl=1,n_tot_fl_modes
             i1_fks = i1_fl + (i1-1)*n_tot_fl_modes
             do i2_fl=1,n_tot_fl_modes
               i2_fks = i2_fl + (i2-1)*n_tot_fl_modes
               !
               do i3_fl=1,n_tot_fl_modes
                 do i4_fl=1,n_tot_duffin_modes
                   !
                   i_shift_comb_fl = (i4_fl-(max_duffin_mode+1)) &
                                  &+ (i3_fl-(max_fl_mode+1)) &
                                  &+ (i2_fl-(max_fl_mode+1)) &
                                  &- (i1_fl-(max_fl_mode+1))
                   !
                   if (abs(i_shift_comb_fl) > max_fl_mode+1) cycle !the +1 ensures both +1 & -1 are out of bounds
                   i_comb_fl = i_shift_comb_fl + (max_fl_mode+1)
                   !
                   if (abs(i_shift_comb_fl) > max_fl_mode-1) then
                     !
                     if (i_shift_comb_fl > 0) then
                       do i_m=1,en%nbf(i_sp) 
                         Wop(i1_fks,i2_fks,istep) = Wop(i1_fks,i2_fks,istep) + & !only plus exponential
                                                  & (conjg(FL_V_bands(i2,i_comb_fl-1,i_m,ik)) * &
                                                  & (FL_V_tilde(i1,i3_fl,i4_fl,i_m,1) - &
                                                  &  FL_V_tilde(i1,i3_fl,i4_fl,i_m,2)))*ph_p
                       enddo
                     else
                       do i_m=1,en%nbf(i_sp) 
                         Wop(i1_fks,i2_fks,istep) = Wop(i1_fks,i2_fks,istep) + & !only minus exponential
                                         & (-1._SP)*(conjg(FL_V_bands(i2,i_comb_fl+1,i_m,ik)) * &
                                                  & (FL_V_tilde(i1,i3_fl,i4_fl,i_m,1) - &
                                                  &  FL_V_tilde(i1,i3_fl,i4_fl,i_m,2)))*ph_m
                       enddo
                     endif
                   else
                     do i_m=1,en%nbf(i_sp) 
                       Wop(i1_fks,i2_fks,istep) = Wop(i1_fks,i2_fks,istep) + & !both exponentials
                                                & (conjg(FL_V_bands(i2,i_comb_fl-1,i_m,ik)) * & !plus exp
                                                & (FL_V_tilde(i1,i3_fl,i4_fl,i_m,1) - &
                                                &  FL_V_tilde(i1,i3_fl,i4_fl,i_m,2)))*ph_p + &
                                       & (-1._SP)*(conjg(FL_V_bands(i2,i_comb_fl+1,i_m,ik)) * & !minus exp
                                                & (FL_V_tilde(i1,i3_fl,i4_fl,i_m,1) - &
                                                &  FL_V_tilde(i1,i3_fl,i4_fl,i_m,2)))*ph_m
                     enddo
                   endif
                   !
                 enddo ! i4_fl
               enddo ! i3_fl
               !
             enddo ! i2_fl
           enddo ! i1_fl
         enddo ! i2 band
       enddo ! i1 band
       !
       do i1=H_ref_bands(1),H_ref_bands(2)
         do i1_fl=1,n_tot_fl_modes
           !
           i1_fks = i1_fl + (i1-1)*n_tot_fl_modes
           Wop(i1_fks,:,istep) = &
         & Wop(i1_fks,:,istep)* &
         & cI*k_map%max_kdir(id_red)/(4._SP*pi*real(istep,SP)) / (2._SP*cI)
           !
         enddo
       enddo
       !
     enddo
     !
   else ! non-periodic directions 
     !
     ! <\phi_i | e^{iGr} | \phi_j >
     !
     Wop(:,:,:)=cZERO !DIP_S(H_ref_bands(1):,H_ref_bands(1):,id_red,ik,i_sp)*cI/(4._SP*pi)   
     !
   endif    
   !
   ! Transform in cartesian coordinates 
   !
   if(max_step==1) then
     !      
     ! First order formula D(dk)      
     ! Eq. 96 of PRB 63, 155107(2001)
     !
     do id=1,3
       Wk(id,:,:)=Wk(id,:,:)+Wop(:,:,1)*a(id_red,id)
     enddo
     !
   else
     !      
     ! Second order formula   ( 4*D(dk) - D(2*dk) ) / 3
     ! Eq. 97 of PRB 63, 155107(2001)
     !
     do id=1,3
       Wk(id,:,:)=Wk(id,:,:)+(4._SP*Wop(:,:,1)-Wop(:,:,2))/3._SP*a(id_red,id)
     enddo
     !
   endif
   !
 enddo ! loop on id_red
 !
 ! Make an Hermitian operator
 !
 do id=1,3
   Wk(id,FL_states(1):FL_states(2),FL_states(1):FL_states(2)) = &
  &Wk(id,FL_states(1):FL_states(2),FL_states(1):FL_states(2)) + &
  &hermitian(Wk(id,FL_states(1):FL_states(2),FL_states(1):FL_states(2)))
 enddo
 !
  do i1=H_ref_bands(1),H_ref_bands(2)
    do i1_fl=1,n_tot_fl_modes
      i1_fks = i1_fl + (i1-1)*n_tot_fl_modes
      do id=1,3
        H_nl_sc(i1_fks,:)=H_nl_sc(i1_fks,:) - &
 &      Efield(1)%amplitude*Efield(1)%versor(id)*Wk(id,i1_fks,:)
      enddo
    enddo
  enddo
 !
end subroutine FL_build_W_operator