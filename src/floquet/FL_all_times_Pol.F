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
subroutine FL_all_times_pol(E,k)
 !
 ! Subroutine to interace with ypp
 !
 use pars,           ONLY:DP,SP,cZERO,pi,cONE,cI
 use units,          ONLY:HA2FSm1,HA2EV,SPEED_OF_LIGHT
 use electrons,      ONLY:levels,n_sp_pol,spin_occ,n_spin
 use fields,         ONLY:Efield,Divide_by_Field,A_ext
 use D_lattice,      ONLY:a,DL_vol
 use R_lattice,      ONLY:bz_samp,k_map,bz_map
 use vec_operate,    ONLY:get_id_perpendicular,k_periodic_idx
 use electric,       ONLY:l_force_SndOrd
 use QP_m,           ONLY:QP_nk
 use real_time,      ONLY:RT_step,NE_steps,NE_i_time,RT_nbands,RT_bands,eval_DFT
 use nl_optics,      ONLY:NL_bands,NL_initial_P,NL_damping,NL_P_t,E_tot,E_ext,E_ks
 use fl_optics,      ONLY:max_fl_mode,n_tot_fl_modes,FL_space_dim,FL_S_plus,FL_S_minus,&
&                         FL_states,FL_Ho_plus_Sigma,FL_Chi_order,&
&                         max_time_step
 !
 implicit none
 !
 type(levels),  intent(in)  :: E
 type(bz_samp), intent(in)  :: k
 !
 ! Work Space
 !
 complex(DP)  :: FL_all_time_S_det(NE_steps,6,QP_nk,n_sp_pol)
 complex(DP)  :: FL_time_P_Berry_RED(3,NE_steps)  ! In reduced coordinates
 complex(DP)  :: FL_time_P_Berry_CART(3,NE_steps) ! In cartesian coordinates
 integer      :: ik_bz,is,i_t,i_order,i1_fl
 integer      :: id_in,id2,id3,Nperpend,i1,i2,i3,idx(3),ikbz,id
 integer      :: ik_ibz,max_step,istep,ikp
 complex(DP)  :: zeta
 real(DP)     :: imag_zeta
 real(SP)     :: time,w0_sm1
 character(4) :: filename
 integer,allocatable      :: phase_correction(:,:)
 !
 ! Update Floquet Overlaps and determinant
 ! ===================================================================
 !
 FL_all_time_S_det =cONE
 !
 do ik_bz=1,QP_nk
   !
   !if(.not.PAR_IND_Xk_bz%element_1D(ik_bz)) cycle
   !
   do is=1,n_sp_pol
     !
     do id=1,3
       !
       ik_ibz=k%sstar(ik_bz,1)
       !
       if(k_map%max_kdir(id)/=1) then ! periodic directions
         !
         max_step=1
         if(k_map%max_kdir(id)>=6.and..not.l_force_SndOrd) max_step=2
         !
         do istep=1,max_step
           idx=k_map%k_map_inv(ik_bz,:)
           idx(id)=idx(id)+istep
           idx=k_periodic_idx(idx,k_map)
           ikp=k_map%k_map_dir(idx(1),idx(2),idx(3))
           !
           call FL_all_times_determinants(E,ik_bz,ikp,is,FL_S_plus(:,:E%nbf(is),:E%nbf(is),id+(istep-1)*3,ik_bz,is),&
           &                                            FL_all_time_S_det(:,id+(istep-1)*3,ik_bz,is))
         enddo
       endif
     enddo
   enddo
   !
 enddo
 !
 FL_time_P_Berry_RED  = cZERO
 FL_time_P_Berry_CART = cZERO
 !
 do id_in=1,3
   !
   call get_id_perpendicular(id_in,id2,id3)
   !
   Nperpend=k_map%max_kdir(id2)*k_map%max_kdir(id3)
   !
   allocate(phase_correction(k_map%max_kdir(id2),k_map%max_kdir(id3)))
   do i_t=1,NE_steps
     !
     do i2=1,k_map%max_kdir(id2)
       do i3=1,k_map%max_kdir(id3)
         !
         zeta=cONE
         !
         idx(id2)=i2
         idx(id3)=i3
         !
         ! Loop on the string
         !
         do i1=1,k_map%max_kdir(id_in)
           !
           idx(id_in)=i1
           ikbz=k_map%k_map_dir(idx(1),idx(2),idx(3))
           zeta=zeta*FL_all_time_S_det(i_t,id_in,ikbz,1)
           !
         enddo
         !
         ! Map phases between -pi and pi for all k_perpendicular
         ! Do we miss a factor 2?
         !
         imag_zeta=aimag(log(zeta))
         if (i_t == 1) phase_correction(i2,i3) = nint(imag_zeta/(1._DP*pi))
         imag_zeta=imag_zeta-1._DP*pi*phase_correction(i2,i3)
         !
         FL_time_P_Berry_RED(id_in,i_t)=FL_time_P_Berry_RED(id_in,i_t)+imag_zeta
         !
       enddo
     enddo
     !
     FL_time_P_Berry_RED(id_in,i_t)=-FL_time_P_Berry_RED(id_in,i_t)/real(Nperpend,SP)
     !
   enddo
   !
   deallocate(phase_correction)
   !
 enddo
 !
 !
 do id=1,3
   !
   do i_t=1,NE_steps
     !
     NL_P_t(i_t,id)=sum(FL_time_P_Berry_RED(:,i_t)&
             &                   *a(:,id))/(DL_vol*2._SP*pi)*spin_occ&
             &                   -NL_initial_P(id)
     !
   enddo
   !
 enddo
 !
 !
!aim137 DEBUG <
write(filename,'(f4.2)') Efield(1)%frequency(1)*HA2EV
w0_sm1 = Efield(1)%frequency(1)*HA2FSm1
open(111,file='FL_time_Pol_at_w_'//trim(adjustl(filename))//'.dat')
write(111,*) 'Frequency[eV] = ',Efield(1)%frequency(1)*HA2EV
write(111,*) 't_step  time   Pol_x   Pol_y   Pol_z'
do i_t=1,NE_steps
time=real(i_t-1,SP)*RT_step
!time = 1/(w0_sm1) * (i_t-1)/NE_steps ! when i_t=1, time = 0
write(111,*) i_t, time, real(NL_P_t(i_t,1)),real(NL_P_t(i_t,2)),real(NL_P_t(i_t,3))
enddo
close(111)
!aim137 DEBUG >
end subroutine
!
subroutine FL_all_times_determinants(en,ikbz,ikp,i_sp,FL_S_plus,FL_all_time_S_det)
 !
 use pars,           ONLY:DP,SP,cZERO,pi,cONE,cI
 use units,          ONLY:HA2FSm1,HA2EV
 use electrons,      ONLY:levels,n_sp_pol,spin_occ,n_spin
 use fields,         ONLY:Efield,EtoT
 use real_time,      ONLY:RT_step,NE_steps,NE_i_time,RT_nbands,RT_bands,eval_DFT
 use nl_optics,      ONLY:NL_bands,NL_damping
 use fl_optics,      ONLY:max_fl_mode,max_extra_mode,n_tot_fl_modes,FL_states,&
&                         n_tot_extra_modes,FL_QE
 !
 implicit none
 !
 type(levels), intent(in)   :: en
 integer,     intent(in)    :: ikbz,ikp,i_sp
 complex(SP), intent(inout) :: FL_S_plus(en%nbf(i_sp),en%nbf(i_sp),n_tot_extra_modes)
 complex(DP), intent(inout) :: FL_all_time_S_det(NE_steps)
 !
 ! Work Space
 !
 complex(SP)   :: FL_time_S_plus(en%nbf(i_sp),en%nbf(i_sp),NE_steps)
 integer       :: i_t,i1_extra,i1_shifted_fl,i1,i2
 real(SP)      :: time,w0,T_period,t_samples(NE_steps)
 !
 !
 FL_time_S_plus =cZERO 
 FL_all_time_S_det  =cZERO
 !
 ! Calculate S plus as a function of time
 ! ======================================
 !
 w0 = Efield(1)%frequency(1) ! in Ha
 !
 do i_t=1,NE_steps
   !
   time=real(i_t-1,SP)*RT_step
   !
   do i1_extra=1,n_tot_extra_modes
     !
     i1_shifted_fl = i1_extra -(max_extra_mode+1)
     !
     FL_time_S_plus(:,:,i_t)  = FL_time_S_plus(:,:,i_t) &
                            & + exp(-1*cI*real(i1_shifted_fl,SP)*w0*time) &
                            & * FL_S_plus(:,:,i1_extra)
                           !& + exp(-1._SP*cI*i1_shifted_fl*w0*t_samples(i_t)) &
                           !aim137 I compared these two, and the former is better numerically
     !
   enddo
 ! print*, exp(-1*cI*2*pi*(i_t-1)/NE_steps)
   !
   ! Add Quasi-energies exponentials
   ! ================================
   do i1=1,en%nbf(i_sp)
     !
     do i2=1,en%nbf(i_sp)
       !
       FL_time_S_plus(i1,i2,i_t) = FL_time_S_plus(i1,i2,i_t)     &
                               &  * exp(+cI*FL_QE(i1,ikbz,1)*time) &
                               &  * exp(-cI*FL_QE(i2,ikp ,1)*time)
       !
     enddo
     !
   enddo
   !
   ! Calculate determinant as a function of time
   ! ============================================
   !
   call SERIAL_inversion(en%nbf(i_sp),FL_time_S_plus(:,:,i_t),FL_all_time_S_det(i_t),.true.)
   !
 enddo
 !
end subroutine
