!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS AM
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
subroutine RT_occ_bands_interpolation(en,k,n_T_steps)
 !
 use pars,           ONLY:SP,schlen,rZERO
 use electrons,      ONLY:levels,n_sp_pol,n_spinor,E_reset,n_spin
 use R_lattice,      ONLY:bz_samp
 use YPPm,           ONLY:BANDS_steps,INTERP_grid,CIRCUIT_k,BANDS_path,CIRCUIT_E_DB_path,&
&                         CIRCUIT_made_of_SPECIAL_k,BANDS_bands,&
&                         l_PROJECT_line,l_PROJECT_plane,PROJECT_depth
 use YPP_real_time,  ONLY:l_RT_lifetimes,l_RT_occupations,RT_time,integrated_Nel,integrated_Nhole
 use YPP_interfaces, ONLY:PROJECTION_setup,PROJECTION_k_on_the_path_or_plane,PROJECTION_plot,k_special
 use stderr,         ONLY:intc
 use RT_control,     ONLY:RT_apply,RT_carriers_object,RT_carriers_to_RT_k,RT_carriers_to_RT_E
 use real_time,      ONLY:RT_carriers,RT_bands
 use interpolate,    ONLY:INTERPOLATION_driver_seed,INTERPOLATION_driver_do,electrons_bands_interpolate,&
&                         INTERP_obj,INTERP_mode,GRID_k
 use LIVE_t,         ONLY:live_timing
 !
#include<memory.h>
 !
 integer,       intent(in)    :: n_T_steps
 type(bz_samp), intent(in)    :: k
 type(levels),  intent(inout) :: en
 !
 ! Work space
 !
 type(bz_samp)         :: RT_k
 type(levels)          :: CIRCUIT_E,CIRCUIT_dE,RT_E
 real(SP)              :: Max_occ
 real(SP), allocatable :: integrated_dF_elecs(:,:,:),integrated_dF_holes(:,:,:)
 character(schlen)     :: operation
 logical               :: WARN_neg_elecs_df,WARN_pos_holes_df,is_PROJECTED,l_update_E,QUIET_interp
 integer               :: ib,ik,i_RT,i_T,ID_circuit,ID_spin,ID_magn,ID_bands(3),ID_grid,ik_path
 logical, external     :: file_exists
 !
 call section('*','Interpolation tool')
 !=====================================
 !
 l_update_E=.false.
 !
 if(all(INTERP_grid>0).or.BANDS_steps<=0) then
   call error(" Interpolation of occupations/lifetimes only on a band circuit")
 endif
 !
 if(n_sp_pol>1) then
   call error('Occupations for spin polarized systems not implemented')
 endif
 !
 if (l_RT_lifetimes .and. .not.allocated(RT_carriers%NEQ_Lifetime)  ) then
   call error('No scattering => no Lifetimes')
 endif
 !
 if (BANDS_bands(1)<RT_bands(1).or.BANDS_bands(2)>RT_bands(2)) then
   BANDS_bands(1)=max(BANDS_bands(1),RT_bands(1))
   BANDS_bands(2)=min(BANDS_bands(2),RT_bands(2))
   call warning("User bands restricted to "//trim(intc(BANDS_bands(1)))//" - "//trim(intc(BANDS_bands(2))))
 endif
 !
 if (file_exists(trim(CIRCUIT_E_DB_path)//"/SAVE/ns.db1")) BANDS_path= " "
 !
 ! Define the circuit
 !====================
 call k_special( )
 call k_circuit_driver( BANDS_path, BANDS_steps )
 !
 CIRCUIT_made_of_SPECIAL_k=CIRCUIT_k%nbz>0
 !
 ! Projection
 !============
 call PROJECTION_setup("setup")
 call PROJECTION_setup( "BANDS", BANDS_bands )
 !
 is_PROJECTED=l_PROJECT_line.or.l_PROJECT_plane
 if (is_PROJECTED) then
   YAMBO_ALLOC(integrated_dF_elecs,(GRID_k%nbz/PROJECT_depth,n_T_steps,n_spin))
   YAMBO_ALLOC(integrated_dF_holes,(GRID_k%nbz/PROJECT_depth,n_T_steps,n_spin))
   integrated_dF_elecs=rZERO
   integrated_dF_holes=rZERO
 endif
 !
 call section('=',"Interpolation")
 !================================
 !
 ! Map the RT k in RT_k...
 call RT_carriers_to_RT_k(RT_k)
 call k_ibz2bz(RT_k,'iku',.TRUE.)
 !
 if (.not.l_update_E) then
   !
   call RT_carriers_to_RT_E(en,RT_E,"Eo")
   !
   ! Perform the energies interpolation
   !====================================
   call electrons_bands_interpolate(RT_E,RT_k,RT_bands,ID_bands,CIRCUIT_E,CIRCUIT_dE=CIRCUIT_dE)
   !
 endif
 !
 ! Perform interpolation of spinorial factors and magnetization
 !==============================================================
 if(n_spinor>1) call electrons_spin_and_magn_interpolate("S",k,CIRCUIT_k,RT_bands,ID_spin,ID_magn)
 !
 ! Perform the occupations interpolation
 !======================================
 !
 YAMBO_ALLOC(integrated_Nel,(n_T_steps,2))
 YAMBO_ALLOC(integrated_Nhole,(n_T_steps,2))
 integrated_Nel  =rZERO
 integrated_Nhole=rZERO
 !
 WARN_neg_elecs_df=.FALSE.
 WARN_pos_holes_df=.FALSE.
 !
 QUIET_interp=trim(INTERP_mode)=="NN"
 if (QUIET_interp) call live_timing('Time Interpolation(s)',n_T_steps)
 !
 do i_T=1,n_T_steps
   !
   call RT_apply(RT_bands,en,k,Time=RT_time(i_T),keep_RT_carriers=.TRUE.)
   !
   ! RT_carriers -> RT_E (E components)
   !
   call RT_carriers_to_RT_E(en,RT_E,"E")
   !
   if (l_update_E) then
     !
     ! Perform the energies interpolation
     !====================================
     call electrons_bands_interpolate(RT_E,RT_k,RT_bands,ID_bands,CIRCUIT_E,CIRCUIT_dE=CIRCUIT_dE)
     !
   endif
   !
   if (is_PROJECTED) then
     if (l_RT_occupations) operation="df_on_grid"
     if (l_RT_lifetimes)   operation="W_on_grid"
   else
     if (l_RT_occupations) operation="df_on_circuit"
     if (l_RT_lifetimes)   operation="W_on_circuit"
   endif
   operation=trim(operation)//" Time "//trim(intc(i_T))//"/"//trim(intc(n_T_steps))
   !
   if (is_PROJECTED) then
     call INTERPOLATION_driver_seed(trim(operation),ID_grid,RT_bands,RT_k,SEED_RT=RT_carriers,QUIET=QUIET_interp)
     call INTERPOLATION_driver_do(trim(operation),ID_grid,GRID_k)
     if (l_RT_occupations) call EVAL_integrated_occupations(ID_grid)
   else
     call INTERPOLATION_driver_seed(trim(operation),ID_circuit,RT_bands,RT_k,SEED_RT=RT_carriers,QUIET=QUIET_interp)
     call INTERPOLATION_driver_do(trim(operation),ID_circuit,CIRCUIT_k)
     if (l_RT_occupations) call EVAL_integrated_occupations(ID_circuit)
   endif
   !
   do i_RT=1,RT_carriers%nstates
     ib      =RT_carriers%table(i_RT,1)
     ik      =RT_carriers%table(i_RT,3)
     if (ib<=RT_E%nbf) integrated_Nhole(i_T,1)=integrated_Nhole(i_T,1)-RT_carriers%delta_f(i_RT)*RT_k%weights(ik)
     if (ib> RT_E%nbf) integrated_Nel(i_T,1)  =integrated_Nel(i_T,1)+RT_carriers%delta_f(i_RT)*RT_k%weights(ik)
   enddo
   Max_occ=maxval(abs(RT_carriers%delta_f))
   !
   if (is_PROJECTED) then
     !
     do ik=1,GRID_k%nbz
       ik_path=PROJECTION_k_on_the_path_or_plane(ik)
       if (ik_path==0) cycle
       do ib=BANDS_bands(1),BANDS_bands(2)
         if (ib<=RT_E%nbf) then
           integrated_dF_holes(ik_path,i_T,:)=integrated_dF_holes(ik_path,i_T,:)-&
&                                             GRID_k%weights(ik)*INTERP_obj(ID_grid)%INTERP(ib,ik,1)
         else
           integrated_dF_elecs(ik_path,i_T,:)=integrated_dF_elecs(ik_path,i_T,:)+&
&                                             GRID_k%weights(ik)*INTERP_obj(ID_grid)%INTERP(ib,ik,1)
         endif
       enddo
     enddo
     !
   endif
   !
   ! OUTPUT
   !=======
   if (.not.is_PROJECTED) call RT_plot_interpolated_values(CIRCUIT_E,RT_E,CIRCUIT_dE,i_T,ID_circuit,ID_spin,&
   &                                                             Max_occ,WARN_neg_elecs_df,WARN_pos_holes_df)
   !
   if (     is_PROJECTED) call INTERPOLATION_driver_end(ID_grid)
   if (.not.is_PROJECTED) call INTERPOLATION_driver_end(ID_circuit)
   !
   if (QUIET_interp) call live_timing(steps=1)
   !
 enddo
 !
 if (QUIET_interp) call live_timing()
 !
 if (WARN_neg_elecs_df) call warning("[INTERPOLATION] Found negative electron occupations")
 if (WARN_pos_holes_df)     call warning("[INTERPOLATION] Found positive hole occupations")
 !
 ! Projection Output
 !===================
 !
 if (l_PROJECT_line.or.l_PROJECT_plane) then 
   !
   if (l_RT_lifetimes  ) operation="lifetimes"
   if (l_RT_occupations) operation="occupations"
   ! 
   call PROJECTION_plot(trim(operation)//"_electrons", n_T_steps, (/1,1/), OBS_K=integrated_dF_elecs, TIME=RT_time ) 
   call PROJECTION_plot(trim(operation)//"_holes", n_T_steps, (/1,1/), OBS_K=integrated_dF_holes, TIME=RT_time ) 
   !
 endif
 !
 ! Clean
 !
 if(n_spinor>1) call INTERPOLATION_driver_end(ID_spin)
 call INTERPOLATION_driver_end(0)
 call RT_carriers_object(RT_carriers,WHAT='free')
 call E_reset(CIRCUIT_E)
 call E_reset(RT_E)
 !
 contains
   !
   subroutine EVAL_integrated_occupations(ID)
     integer  :: ID
     real(SP) :: NK
     !
     if (ID==ID_circuit) NK=real(CIRCUIT_k%nbz)
     if (ID==ID_grid)    NK=real(GRID_k%nbz)
     !
     integrated_Nhole(i_T,2)=-sum(INTERP_obj(ID)%INTERP(RT_bands(1):RT_E%nbf,:,:))/NK
     integrated_Nel(i_T,2)  = sum(INTERP_obj(ID)%INTERP(RT_E%nbf+1:RT_bands(2),:,:))/NK
     !
   end subroutine
   !
end subroutine RT_occ_bands_interpolation
