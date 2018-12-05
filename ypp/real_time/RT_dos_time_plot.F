! 
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine RT_dos_time_plot(En,k,n_T_steps)
 !
 use units,          ONLY:HA2EV,AUT2FS
 use pars,           ONLY:SP,schlen
 use D_lattice,      ONLY:DL_vol
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,n_spin,n_spinor
 use YPP,            ONLY:DOS_broadening,DOS_E_steps,electrons_dos_elemental,INTERP_grid,DOS_E_step
 use QP_CTL_m,       ONLY:QP_apply
 use YPP_real_time,  ONLY:RT_time
 use IO_m,           ONLY:IO_and_Messaging_switch
 use com,            ONLY:msg
 use functions,      ONLY:Fermi_fnc_derivative
 use LIVE_t,         ONLY:live_timing
 use RT_control,     ONLY:RT_output,RT_apply,RT_carriers_to_RT_k,RT_carriers_to_RT_E
 use real_time,      ONLY:RT_carriers,RT_bands
 use parser_m,       ONLY:parser
 use interpolate,    ONLY:GRID_k,INTERPOLATION_driver,INTERP_obj
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
 real(SP),allocatable  :: spin_factors(:,:,:),RT_spin_factors(:,:,:)
 real(SP),   external  :: RIntegrate
 integer               :: i_E,i_T,ib,N_files,i_f,i_RT,ID_E,ID_f,ID_spin
 logical               :: l_sum_equilibrium,l_separate_eh,space_printed,l_interp,l_skip_occ
 type(levels)          :: RT_E,GRID_E
 type(bz_samp)         :: RT_k
 character(schlen)     :: file_n(2)
 !
 call section('*','Electr. Time-dependent DOS')
 !
 ! Setup
 !
 call electrons_dos_setup( En, dos_E, RT_bands )  
 !
 call parser('SeparateEH',l_separate_eh)
 call parser('IncludeEQocc',l_sum_equilibrium)
 call parser('NoOcc',l_skip_occ)
 !
 N_files=1
 l_interp=all(INTERP_grid>0)
 if (l_interp) N_files=2
 !
 ! Msgs
 !
 call msg('s','Energy steps   :',DOS_E_steps)
 call msg('s','Bands          :',RT_carriers%nb)
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
 if (n_spinor==2) then
   YAMBO_ALLOC(spin_factors,(RT_carriers%nb(1):RT_carriers%nb(2),k%nibz,n_spinor))
   YAMBO_ALLOC(RT_spin_factors,(RT_carriers%nb(1):RT_carriers%nb(2),RT_carriers%nk,n_spinor))
   call electrons_spin_factors((/1,k%nibz/),RT_carriers%nb,spin_factors)
   do i_RT=1,RT_carriers%nstates
     RT_spin_factors(:,RT_carriers%table(i_RT,3),:)=spin_factors(:, RT_carriers%table(i_RT,2), :)
   enddo
 endif 
 !
 ! Map the RT k in RT_k
 !
 call RT_carriers_to_RT_k(RT_k)
 call k_ibz2bz(RT_k,'iku',.TRUE.)
 !
 ! Interpolation GRID INIT 
 !
 if (l_interp) call INTERPOLATION_driver(operation="GRID",GRID=INTERP_grid)
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
   call RT_apply(RT_bands,En,k,Time=RT_time(i_T),keep_RT_carriers=.TRUE.)
   !
   call RT_carriers_to_RT_E(En,RT_E,"E")
   !
   ! Built-in
   !----------
   ! QP corrections
   if (i_T==1) call QP_state_table_setup(RT_E)
   call QP_apply(RT_bands,RT_E,RT_k,'G',msg_fmt='s')
   !
   if(.not.l_skip_occ) then
     if(.not.l_sum_equilibrium) call RT_carriers_to_RT_E(En,RT_E,"df")
     if(     l_sum_equilibrium) call RT_carriers_to_RT_E(En,RT_E,"f")
   endif
   !
   !... DOS
   if (n_spinor==2) then
     call electrons_dos_elemental(RT_k,RT_E,RT_bands,el_dos(:,:,1),&
&                                 dos_E,USE_the_DbGd=.FALSE.,quiet=.TRUE.,FORCE_occ=.not.l_skip_occ,WF_fac=RT_spin_factors)
   else
     call electrons_dos_elemental(RT_k,RT_E,RT_bands,el_dos(:,:,1),&
                                  dos_E,USE_the_DbGd=.FALSE.,quiet=.TRUE.,FORCE_occ=.not.l_skip_occ)
   endif
   !
   if (n_spin==1) el_dos_integral(i_T,1)=RIntegrate(el_dos(:,1,1),dos_E,DOS_E_steps)*DL_vol
   if (n_spin==2) el_dos_integral(i_T,1)=RIntegrate(el_dos(:,1,1)+el_dos(:,2,1),dos_E,DOS_E_steps)*DL_vol
   !
   if (l_interp) then
     call INTERPOLATION_driver(bands=RT_bands,SEED_E=RT_E,SEED_k=RT_k,operation="E_on_RT_grid",QUIET=.TRUE.,ID_op=ID_E)
     call INTERPOLATION_driver(bands=RT_bands,SEED_E=RT_E,SEED_k=RT_k,operation="f_on_RT_grid",QUIET=.TRUE.,ID_op=ID_f)
     call INTERPOLATION_driver(OUT_k=GRID_k,OUT_E=GRID_E,operation="E_on_RT_grid",QUIET=.TRUE.)
     call INTERPOLATION_driver(OUT_k=GRID_k,OUT_E=GRID_E,operation="f_on_RT_grid",QUIET=.TRUE.)
     if (n_spinor==2) then
       call INTERPOLATION_driver(bands=RT_bands,SEED_R3=RT_spin_factors,SEED_k=RT_k,&
&                                operation="SPIN_factors_on_RT_grid",ID_op=ID_spin)
       call INTERPOLATION_driver(OUT_k=GRID_k,operation="SPIN_factors_on_RT_grid")
       !... DOS
       call electrons_dos_elemental(GRID_k,GRID_E,RT_bands,el_dos(:,:,2),dos_E,USE_the_DbGd=.FALSE.,QUIET=.TRUE.,&
&                                   FORCE_occ=.TRUE.,WF_fac=INTERP_obj(ID_spin)%INTERP)
     else
       !... DOS
       call electrons_dos_elemental(GRID_k,GRID_E,RT_bands,el_dos(:,:,2),dos_E,USE_the_DbGd=.FALSE.,QUIET=.TRUE.,&
&                                   FORCE_occ=.TRUE.)
     endif
     !
     if (n_spin==1) el_dos_integral(i_T,2)=RIntegrate(el_dos(:,1,2),dos_E,DOS_E_steps)*DL_vol
     if (n_spin==2) el_dos_integral(i_T,2)=RIntegrate(el_dos(:,1,2)+el_dos(:,2,2),dos_E,DOS_E_steps)*DL_vol
     !
     call INTERPOLATION_driver(operation="END",ID_op=ID_E)
     call INTERPOLATION_driver(operation="END",ID_op=ID_f)
     call INTERPOLATION_driver(operation="END",ID_op=ID_spin)
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
   call RT_output(trim(file_n(i_f)))
 enddo
 !
 call               msg('s','DOS max value            [RT grid]:',el_dos_max(1))
 if (l_interp) call msg('s','                     [INTERP grid]:',el_dos_max(2))
 call               msg('s','Integrated dos (min/max) [RT grid]:',(/minval(el_dos_integral(:,1)),maxval(el_dos_integral(:,1))/))
 if (l_interp) call msg('s','                     [INTERP grid]:',(/minval(el_dos_integral(:,2)),maxval(el_dos_integral(:,2))/))
 !
 call live_timing()
 !
 call INTERPOLATION_driver(operation="END")
 !
 YAMBO_FREE(spin_factors)
 !
end subroutine RT_dos_time_plot
