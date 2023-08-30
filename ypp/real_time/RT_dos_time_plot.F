! 
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine RT_dos_time_plot(En,k,n_T_steps)
 !
 use units,          ONLY:HA2EV,AUT2FS
 use pars,           ONLY:SP,schlen
 use D_lattice,      ONLY:DL_vol
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,n_spin,n_spinor,n_sp_pol
 use YPP_interfaces, ONLY:electrons_dos_elemental
 use YPPm,           ONLY:DOS_broadening,DOS_E_steps,INTERP_grid,DOS_E_step,l_PROJECT_atom,&
&                         l_add_EQ_occ,l_separate_eh,l_skip_occ
 use QP_CTL_m,       ONLY:QP_apply
 use YPP_real_time,  ONLY:RT_time
 use IO_int,         ONLY:IO_and_Messaging_switch
 use com,            ONLY:msg
 use LIVE_t,         ONLY:live_timing
 use RT_control,     ONLY:RT_output,RT_apply,RT_carriers_to_RT_k,RT_carriers_to_RT_E
 use real_time,      ONLY:RT_carriers,RT_bands
 use interpolate,    ONLY:GRID_k,INTERPOLATION_driver_seed,INTERPOLATION_driver_do,INTERP_obj
 !
#include<memory.h>
 !
 type(bz_samp) ::k
 type(levels)  ::En
 integer       ::n_T_steps
 !
 ! Work Space
 ! 
 real(SP)              :: el_dos(DOS_E_steps,n_spin,2),dos_E(DOS_E_steps),el_dos_max(2),el_dos_integral(n_T_steps,2)
 real(SP),allocatable  :: TMP_fac(:,:,:)
 real(SP), pointer     :: spin_fac(:,:,:) => null()
 real(SP), pointer     :: pdos_fac(:,:,:) => null()
 real(SP),   external  :: RIntegrate
 integer               :: i_E,i_T,ib,N_files,i_f,i_RT,ID_E,ID_f,ID_spin,ID_pdos
 logical               :: space_printed,l_interp
 type(levels)          :: RT_E,GRID_E
 type(bz_samp)         :: RT_k
 character(schlen)     :: file_n(2)
 !
 call section('*','Electronic Time-dependent DOS')
 !
 ! Setup
 !
 call electrons_dos_setup( En, dos_E, RT_bands )  
 !
 N_files=1
 l_interp=all(INTERP_grid>0)
 if (l_interp) N_files=2
 ID_E=0
 ID_f=0
 ID_spin=0
 ID_pdos=0
 !
 ! Msgs
 !
 call msg('s','Energy steps   ',DOS_E_steps)
 call msg('s','Bands          ',RT_carriers%nb)
 !
 ! Output files headers
 !
 if (n_T_steps==1) then
   file_n(1)="single_time_dos"
   file_n(2)="single_time_INTERPOLATED_dos"
 else
   file_n(1)="TD_dos"
   file_n(2)="TD_INTERPOLATED_dos"
 endif
 call RT_output("open "//trim(file_n(1)))
 if (l_interp) call RT_output("open "//trim(file_n(2)))
 !
 ! Load the PDOS coefficient
 !-----------------------------
 if (l_PROJECT_atom) then
   YAMBO_ALLOC(TMP_fac,(RT_carriers%nb(1):RT_carriers%nb(2),k%nibz,n_sp_pol))
   YAMBO_ALLOC_P(pdos_fac,(RT_carriers%nb(1):RT_carriers%nb(2),RT_carriers%nk,n_sp_pol))
   call electrons_pdos_factors((/1,k%nibz/),RT_carriers%nb,TMP_fac)
   do i_RT=1,RT_carriers%nstates
     pdos_fac(:,RT_carriers%table(i_RT,3),:)=TMP_fac(:, RT_carriers%table(i_RT,2), :)
   enddo
   YAMBO_FREE(TMP_fac)
 endif
 !
 if (n_spinor==2) then
   YAMBO_ALLOC(TMP_fac,(RT_carriers%nb(1):RT_carriers%nb(2),k%nibz,n_spinor))
   YAMBO_ALLOC_P(spin_fac,(RT_carriers%nb(1):RT_carriers%nb(2),RT_carriers%nk,n_spinor))
   call electrons_spin_factors((/1,k%nibz/),RT_carriers%nb,TMP_fac)
   do i_RT=1,RT_carriers%nstates
     spin_fac(:,RT_carriers%table(i_RT,3),:)=TMP_fac(:, RT_carriers%table(i_RT,2), :)
   enddo
   YAMBO_FREE(TMP_fac)
 endif
 !
 ! Map the RT k in RT_k
 !
 call RT_carriers_to_RT_k(RT_k)
 call k_ibz2bz(RT_k,'iku',.TRUE.)
 !
 ! Interpolation GRID 
 !
 if (l_interp) then
   !
   ! ... INIT
   call INTERPOLATION_create_the_grid(INTERP_grid)
   !
   if (l_PROJECT_atom) then
     call INTERPOLATION_driver_seed("PDOS_factors_on_RT_grid",ID_pdos,RT_bands,RT_k,SEED_R3=spin_fac)
     call INTERPOLATION_driver_do("PDOS_factors_on_RT_grid",ID_pdos,GRID_k)
   endif
   !
   if (n_spinor==2) then
     call INTERPOLATION_driver_seed("SPIN_fac_on_RT_grid",ID_spin,RT_bands,RT_k,SEED_R3=pdos_fac)
     call INTERPOLATION_driver_do("SPIN_fac_on_RT_grid",ID_spin,GRID_k)
   endif
   !
 endif
 !
 ! T-D DOS
 !=========
 !
 el_dos_max=0._SP
 !
 call live_timing('TD-DOS',n_T_steps)
 !
 do i_T=1,n_T_steps
   !
   call IO_and_Messaging_switch("-log")
   !
   call RT_apply(RT_bands,En,k,Time=RT_time(i_T),keep_RT_carriers=.TRUE.,skip_interpolation=.TRUE.)
   !
   call RT_carriers_to_RT_E(En,RT_E,"E")
   !
   ! Built-in
   !----------
   ! QP corrections
   if (i_T==1) call QP_state_table_setup(RT_E)
   call QP_apply(RT_bands,RT_E,RT_k,'G',msg_fmt='rs')
   !
   if(.not.l_skip_occ) then
     if(.not.l_add_EQ_occ) call RT_carriers_to_RT_E(En,RT_E,"df")
     if(     l_add_EQ_occ) call RT_carriers_to_RT_E(En,RT_E,"f")
   endif
   !
   !... DOS
   !
   call electrons_dos_elemental(RT_k,RT_E,RT_bands,el_dos(:,:,1),dos_E,spin_fac,pdos_fac,&
&                               USE_the_DbGd=.FALSE.,quiet=.TRUE.,FORCE_occ=.not.l_skip_occ)
   !
   if (n_spin==1) el_dos_integral(i_T,1)=RIntegrate(el_dos(:,1,1),dos_E,DOS_E_steps)*DL_vol
   if (n_spin==2) el_dos_integral(i_T,1)=RIntegrate(el_dos(:,1,1)+el_dos(:,2,1),dos_E,DOS_E_steps)*DL_vol
   !
   if (l_interp) then
     !
     call INTERPOLATION_driver_seed("E_on_RT_grid",ID_E,RT_bands,RT_k,SEED_E=RT_E,QUIET=.TRUE.)
     call INTERPOLATION_driver_do("E_on_RT_grid",ID_E,GRID_k,OUT_E=GRID_E)
     call INTERPOLATION_driver_seed("f_on_RT_grid",ID_f,RT_bands,RT_k,SEED_E=RT_E,QUIET=.TRUE.)
     call INTERPOLATION_driver_do("f_on_RT_grid",ID_f,GRID_k,OUT_E=GRID_E)
     !
     !... DOS
     call electrons_dos_elemental(GRID_k,GRID_E,RT_bands,el_dos(:,:,2),dos_E,spin_fac,pdos_fac,&
&                                 USE_the_DbGd=.FALSE.,QUIET=.TRUE.,FORCE_occ=.TRUE.)
     !
     if (n_spin==1) el_dos_integral(i_T,2)=RIntegrate(el_dos(:,1,2),dos_E,DOS_E_steps)*DL_vol
     if (n_spin==2) el_dos_integral(i_T,2)=RIntegrate(el_dos(:,1,2)+el_dos(:,2,2),dos_E,DOS_E_steps)*DL_vol
     !
     call INTERPOLATION_driver_end(ID_E)
     call INTERPOLATION_driver_end(ID_f)
     !
   endif
   !
   el_dos_max(1)=maxval(abs(el_dos(:,:,1)))
   if (l_interp) el_dos_max(2)=maxval(abs(el_dos(:,:,2)))
   !
   call msg('o dos','')
   !
   ! Output file
   !
   space_printed=.false.
   !
   do i_E=1,DOS_E_steps
     !
     if ( dos_E(i_E)>En%E_VBM(1)+DOS_broadening+epsilon(1._SP) .and. &
&         dos_E(i_E)<En%E_CBm(1)-DOS_broadening-epsilon(1._SP) ) then
       !
       do i_f=1,N_files
         if (l_separate_eh.and.(.not.space_printed)) call RT_output("space "//trim(file_n(i_f)))
       enddo
       !
       space_printed=.true.
       !
       cycle
       !
     endif
     !
     do i_f=1,N_files
       !
       if (n_T_steps> 1) then
         if (n_spin==1) call RT_output(trim(file_n(i_f)),&
&                                      VALUEs=(/el_dos(i_E,1,i_f)/),E=dos_E(i_E)*HA2EV,TIME=RT_time(i_T)*AUT2FS)
         if (n_spin >1) call RT_output(trim(file_n(i_f)),&
&                                      VALUEs=(/el_dos(i_E,1,i_f)+el_dos(i_E,2,i_f),el_dos(i_E,1,i_f),el_dos(i_E,2,i_f)/),&
&                                      E=dos_E(i_E)*HA2EV,TIME=RT_time(i_T)*AUT2FS)
       else
         if (n_spin==1) call RT_output(trim(file_n(i_f)),&
&                                      VALUEs=(/el_dos(i_E,1,i_f)/),E=dos_E(i_E)*HA2EV)
         if (n_spin >1) call RT_output(trim(file_n(i_f)),&
&                                      VALUEs=(/el_dos(i_E,1,i_f)+el_dos(i_E,2,i_f),el_dos(i_E,1,i_f),el_dos(i_E,2,i_f)/),&
&                                      E=dos_E(i_E)*HA2EV)
       endif
       !
       if (n_T_steps> 1.and.i_E==DOS_E_steps) call RT_output("space "//trim(file_n(i_f)))
       !
     enddo
     !
   enddo
   !
   call IO_and_Messaging_switch("+log")
   !
   call live_timing(steps=1)
   !
 enddo
 !
 do i_f=1,N_files
   call RT_output("close "//trim(file_n(i_f)))
 enddo
 !
 call               msg('s','DOS max value            [RT grid]',el_dos_max(1))
 if (l_interp) call msg('s','                     [INTERP grid]',el_dos_max(2))
 call               msg('s','Integrated dos (min/max) [RT grid]',(/minval(el_dos_integral(:,1)),maxval(el_dos_integral(:,1))/))
 if (l_interp) call msg('s','                     [INTERP grid]',(/minval(el_dos_integral(:,2)),maxval(el_dos_integral(:,2))/))
 !
 call live_timing()
 !
 call INTERPOLATION_driver_end(ID_pdos)
 call INTERPOLATION_driver_end(ID_spin)
 !
end subroutine RT_dos_time_plot
