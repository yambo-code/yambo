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
 use pars,           ONLY:SP,rZERO
 use R_lattice,      ONLY:bz_samp,bz_samp_reset
 use D_lattice,      ONLY:DL_vol
 use electrons,      ONLY:levels,n_spin,n_spinor
 use YPP,            ONLY:DOS_broadening,DOS_E_range,DOS_E_steps
 use QP_CTL_m,       ONLY:QP_apply
 use YPP_real_time,  ONLY:RT_time
 use com,            ONLY:msg
 use functions,      ONLY:Fermi_fnc_derivative
 use LIVE_t,         ONLY:live_timing
 use RT_control,     ONLY:RT_output,RT_apply
 use real_time,      ONLY:RT_carriers,RT_bands
 use interfaces,     ONLY:PARALLEL_index
 use parallel_m,     ONLY:PP_indexes,PP_redux_wait,myid
 use parser_m,       ONLY:parser
 use stderr,         ONLY:write_to_log,write_to_log_default
 use YPP,            ONLY:GRID_k,INTERP_grid
 !
#include<memory.h>
 !
 type(bz_samp) ::k
 type(levels)  ::En
 integer       ::n_T_steps
 !
 ! Work Space
 ! 
 real(SP)              :: el_dos(DOS_E_steps,n_spin,2),dos_E,delta_E,f_occ,dos_norm,&
&                         el_dos_max(2)
 real(SP),allocatable  :: spin_factors(:,:,:)
 integer               :: i_RT,i_E,i_sp_pol,i_T,ik,ib
 logical               :: l_sum_equilibrium,l_separate_eh,space_printed,l_interp
 type(levels)          :: RT_E,INTERP_E
 type(bz_samp)         :: RT_k
 type(PP_indexes)      :: px
 !
 call section('*','Electr. Time-dependent DOS')
 !
 ! Setup
 !
 call parser('SeparateEH',l_separate_eh)
 call parser('IncludeEQocc',l_sum_equilibrium)
 l_interp=all(INTERP_grid>0)
 !
 ! Msgs
 !
 call msg('s','Energy steps   :',DOS_E_steps)
 call msg('s','Bands          :',RT_carriers%nb)
 !
 ! Output files headers
 !
 if (n_T_steps==1) call RT_output("open single_time_dos")
 if (n_T_steps> 1) call RT_output("open TD_dos")
 !
 if (DOS_E_range(1)>DOS_E_range(2)) then
   DOS_E_range(1)=minval(RT_carriers%E_bare)-5._SP*DOS_broadening
   DOS_E_range(2)=maxval(RT_carriers%E_bare)+5._SP*DOS_broadening
 endif
 delta_E=(DOS_E_range(2)-DOS_E_range(1))/DOS_E_steps
 !
 if (n_spinor==2) then
   YAMBO_ALLOC(spin_factors,(RT_carriers%nb(1):RT_carriers%nb(2),k%nibz,n_spinor))
   call electrons_spin_factors((/1,k%nibz/),RT_carriers%nb,spin_factors)
 endif 
 !
 ! Map the bare components of RT in RT_E and RT_k
 !
 call RT_occ_bands_kpts_and_En_map(En,k,RT_E,RT_k)
 !
 ! QP corrections
 !
 !call QP_state_table_setup(RT_E)
 !call QP_apply(RT_bands,RT_E,RT_k,'G',msg_fmt='s')
 !
 ! Interpolations INIT 
 !
 if(l_interp) then
   !
   call bz_samp_reset(GRID_k)
   GRID_k%description="Interp"
   !call INTERPOLATION_create_the_grid(GRID_k,'i',INTERP_grid)
   !
   !... Brillouin zone setup
   !call INTERPOLATION_BZ_setup(k)
   !
 endif
 !
 ! Parallel setup and live_timing
 !
 call PARALLEL_index(px,(/DOS_E_steps/))
 call live_timing('DOS',px%n_of_elements(myid+1)*n_T_steps)
 !
 ! Loop
 !
 el_dos_max=0._SP
 !
 do i_T=1,n_T_steps
   !
   !write_to_log=.FALSE.
   !
   call RT_apply(RT_bands,En,k,Time=RT_time(i_T),keep_RT_carriers=.TRUE.)
   !
   do i_RT=1,RT_carriers%nstates
     !
     ib      =RT_carriers%table(i_RT,1)
     ik      =RT_carriers%table(i_RT,3)
     i_sp_pol=RT_carriers%table(i_RT,4)
     !
     RT_E%E(ib,ik,i_sp_pol)=RT_carriers%E_bare(i_RT)
     if(.not.l_sum_equilibrium) RT_E%f(ib,ik,i_sp_pol)=RT_carriers%delta_f(i_RT)
     if(     l_sum_equilibrium) RT_E%f(ib,ik,i_sp_pol)=RT_carriers%delta_f(i_RT)+RT_carriers%f_bare(i_RT)
     !
   enddo
   !
   !if (l_interp) call INTERPOLATION_grid(RT_bands,RT_E,RT_k,INTERP_E,"E f")
   !
   el_dos=rZERO
   !
   do i_E=1,DOS_E_steps
     !
     if (.not.px%element_1D(i_E)) cycle
     !
     dos_E=DOS_E_range(1)+i_E*delta_E

     call el_density_of_states(RT_k,RT_E,dos_E,DOS_broadening,RT_bands,el_dos(i_E,:,1))
     if (all(INTERP_grid>0)) then
       call el_density_of_states(GRID_k,INTERP_E,dos_E,DOS_broadening,RT_bands,el_dos(i_E,:,2))
     endif
!     !
!     do i_RT=1,RT_carriers%nstates
!       !
!       i_b   =RT_carriers%table(i_RT,1)
!       i_k   =RT_carriers%table(i_RT,2)
!       i_spin=RT_carriers%table(i_RT,4)
!       !
!       if(.not.l_sum_equilibrium) f_occ=RT_carriers%delta_f(i_RT)
!       if(     l_sum_equilibrium) f_occ=RT_carriers%delta_f(i_RT)+RT_carriers%f_bare(i_RT)
!       if(n_spinor==2) then
!         el_dos(i_E,:)=el_dos(i_E,:)+f_occ*spin_factors(i_b,i_k,:)*&
!&           Fermi_fnc_derivative(RT_carriers%E_bare(i_RT)-dos_E,DOS_broadening)*dos_norm     
!       else
!         el_dos(i_E,i_spin)=el_dos(i_E,i_spin)+f_occ*&
!&           Fermi_fnc_derivative(RT_carriers%E_bare(i_RT)-dos_E,DOS_broadening)*dos_norm
!       endif
!       !
!     enddo
     !
   enddo
   !
   call PP_redux_wait(el_dos)
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
     dos_E=DOS_E_range(1)+real(i_E,SP)*delta_E
     !
     if ( dos_E>En%E_VBM(1)+DOS_broadening+epsilon(1._SP) .and. &
     &    dos_E<En%E_CBm(1)-DOS_broadening-epsilon(1._SP)         ) then
       if(l_separate_eh.and.(.not.space_printed)) call RT_output("space TD_dos")
       space_printed=.true.
       cycle
     endif
     !
     if (n_T_steps> 1) then
       if (n_spin==1) call RT_output("TD_dos",VALUEs=(/el_dos(i_E,1,1)/),E=dos_E*HA2EV,TIME=RT_time(i_T)*AUT2FS)
       if (n_spin >1) call RT_output("TD_dos",VALUEs=(/el_dos(i_E,1,1)+el_dos(i_E,2,1),el_dos(i_E,1,1),el_dos(i_E,2,1)/),&
&                                             E=dos_E*HA2EV,TIME=RT_time(i_T)*AUT2FS)
     else
       if (n_spin==1) call RT_output("single_time_dos",VALUEs=(/el_dos(i_E,1,1)/),E=dos_E*HA2EV)
       if (n_spin >1) call RT_output("single_time_dos",VALUEs=(/el_dos(i_E,1,1)+el_dos(i_E,2,1),el_dos(i_E,1,1),el_dos(i_E,2,1)/),&
&                                                      E=dos_E*HA2EV)
     endif
     !
   enddo
   !
   if (n_T_steps> 1) call RT_output("space TD_dos")
   !
   write_to_log=write_to_log_default
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call               msg('s','DOS max value      [RT grid]:',el_dos_max(1))
 if (l_interp) call msg('s','               [INTERP grid]:',el_dos_max(2))
 !
 call live_timing()
 !
 YAMBO_FREE(spin_factors)
 !
end subroutine RT_dos_time_plot
