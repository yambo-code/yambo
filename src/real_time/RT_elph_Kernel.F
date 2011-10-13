!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine RT_elph_kernel(ik,E,k,q,Time)
 !  
 use pars,           ONLY:SP
 use units,          ONLY:HBAR_eVfs,FS2AUT,HA2EV
! use wrapper,        ONLY:M_by_M,V_by_V_plus_V
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp,qindx_S
 use SC,             ONLY:SC_bands
!,V_hartree_sc,Ho,H_nl_sc,rho_n,V_xc_sc,local_V,it_now,SC_R
 use real_time,      ONLY:NE_MEM_steps
!I1_lesser,I2_lesser,l_NE_with_fields,RAD_LifeTime,full_rho,           &
!&                         Thermal_steps,SwitchOnTime,RT_step,XC0_and_HARTREE0,rho_reference,   & 
!&                         G_lesser_reference,Phase_LifeTime,NE_Hxc_jumps,       &
!&                         RT_is_dumped,l_NE_simulate_IP,Hfrozen,XCfrozen,NE_dynamics,it_start, &
!&                         RT_Deph_Matrix,l_RT_needs_XC0
! use fields,         ONLY:gauge_field
! use wave_func,      ONLY:wf
! use xc_functionals, ONLY:V_xc
! use global_XC,      ONLY:WF_kind,WF_xc_functional 
! use par_proc_m,     ONLY:pp_redux_wait,pp_indexes,pp_indexes_reset
! use par_indexes_m,  ONLY:par_indexes  
! use SC,             ONLY:SC_xc_functional
! use xc_functionals, ONLY:XC_LDA_C_PZ
 !
 implicit none
 !
 type(levels),  intent(in)    :: E
 type(bz_samp), intent(in)    :: k,q
 integer,       intent(in)    :: ik
 real(SP),      intent(in)    :: Time
 !
 ! Work Space
 !
 integer       :: iqbz,iq,iqs,ik_m_q
 complex(SP)   :: L_gl(SC_bands(2),SC_bands(2),NE_MEM_steps)
 complex(SP)   :: L_lg(SC_bands(2),SC_bands(2),NE_MEM_steps)
 complex(SP), external :: RT_G_retarded
 !
 !write (100,'(3f20.10)') Time/FS2AUT,RT_G_retarded(E,SC_bands(1),1,Time)
 !return
 !
 do iqbz=1,q%nbz
   !
   iq =q%sstar(iqbz,1)
   iqs=q%sstar(iqbz,2)
   ik_m_q=qindx_S(ik,iqbz,1)
   !
   call RT_generalized_KBA(L_gl,ik,ik_m_q,Time,'>','<')
   call RT_generalized_KBA(L_lg,ik,ik_m_q,Time,'<','>')
   !
 enddo
 !
end subroutine RT_elph_kernel

