!
!        Copyright (C) 2000-2021 the YAMBO team
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
subroutine RT_transient_absorption(Xen,Xk,Xq)
 !
 use pars,            ONLY:SP,pi,cZERO,rZERO
 use R_lattice,       ONLY:bz_samp,q0_def_norm,d3k_factor
 use electrons,       ONLY:levels,spin,spin_occ
 use LIVE_t,          ONLY:live_timing
 use vec_operate,     ONLY:v_norm,v_rotate
 use BS,              ONLY:l_BS_kerr
 use RT_control,      ONLY:RT_apply,CARR_RT_IO_t
 use YPP_real_time,   ONLY:RT_conf,RT_time,TRabsDIP_dir,TRabsWHAT,TRabsDIP_plane,&
&                          TRabs_RES_left,TRabs_RES_right,TRabsMODE,TRabs_E
 use YPPm,            ONLY:DIPs
 use X_m,             ONLY:Epsilon_ii,global_gauge
 use IO_m,            ONLY:LOG
 use BS,              ONLY:BS_bands,BS_H_dim
 use frequency,       ONLY:w_samp
 use parallel_int,    ONLY:PARALLEL_global_indexes
 use real_time,       ONLY:RT_bands,l_TRabs_YPP
 use stderr,          ONLY:STRING_match,STRING_same
 use com,             ONLY:msg
 !
#include<memory.h>
 !
 type(levels)      :: Xen
 type(bz_samp)     :: Xk,Xq
 !
 ! Work space
 !
 type(w_samp)            :: W_bss
 integer                 :: N_eq_trans,N_cc_neq_trans,N_vv_neq_trans,N_trans,N_trans_done,i_T,io_err
 real(SP)                :: DIP_kerr_dir(3),Co
 !
 ! Pre-setup of carrier components and Time evolution parameters
 !
 call RT_apply(DIPs%ib,Xen,Xk,IO_COM=LOG,TIMES_ONLY=.TRUE.)
 !
 ! Setup of Time-range & Report
 !
 call RT_time_configuration_setup("CARRIERS",CARR_RT_IO_t)
 !
 ! Dimensions, Constant, Logicals and allocs
 !
 l_TRabs_YPP=.TRUE.
 l_BS_kerr  =.TRUE.
 DIPs%q0=TRabsDIP_dir
 if (BS_H_dim> 0) then
   DIPs%ib   =BS_bands
   N_eq_trans=BS_H_dim
 else
   DIPs%ib   =RT_bands
   N_eq_trans=(RT_bands(2)-Xen%nbf)*(Xen%nbf-RT_bands(1)+1)*Xk%nbz
 endif
 N_cc_neq_trans=(Xen%nbf-RT_bands(1)+1)*(Xen%nbf-RT_bands(1)+1)*Xk%nbz
 N_vv_neq_trans=(RT_bands(2)-Xen%nbf)*(RT_bands(2)-Xen%nbf)*Xk%nbz
 N_trans=0
 if (STRING_match(TRabsMODE,"eq")) N_trans=N_eq_trans
 if (STRING_match(TRabsMODE,"vv")) N_trans=N_trans+N_vv_neq_trans
 if (STRING_match(TRabsMODE,"cc")) N_trans=N_trans+N_cc_neq_trans
 !
 call msg('s','[TR-ABS] EQ transitions     :',N_eq_trans)
 call msg('s','[TR-ABS] NEQ cc transitions :',N_cc_neq_trans)
 call msg('s','[TR-ABS] NEQ vv transitions :',N_vv_neq_trans)
 !
 Co=real(spin_occ)/(2._SP*pi)**3*d3k_factor*4._SP*pi/q0_def_norm**2
 !
 YAMBO_ALLOC(TRabs_E,(N_eq_trans))
 YAMBO_ALLOC(TRabs_RES_right,(N_eq_trans))
 YAMBO_ALLOC(TRabs_RES_left, (N_eq_trans))
 !
 ! Par Setup
 !
 call PARALLEL_global_indexes(Xen,Xk,Xq," ",RESET=.TRUE.)
 call PARALLEL_global_indexes(Xen,Xk,Xq,"DIPOLES",Dip=DIPs)
 !
 ! Dipoles
 !
 call DIPOLE_IO(Xk,Xen,DIPs,'read',io_err,'DIP')
 if (trim(global_gauge)=='length'  ) DIPs%q0=DIPs%q0/v_norm(DIPs%q0)*q0_def_norm
 if (trim(global_gauge)=='velocity') DIPs%q0=DIPs%q0/v_norm(DIPs%q0)
 DIP_kerr_dir=rZERO
 if (.not.STRING_same(TRabsDIP_plane,"none").and.STRING_match(TRabsWHAT,"k")) then
   call v_rotate(trim(TRabsDIP_plane),-pi/2._SP,DIPs%q0,v_out=DIP_kerr_dir)
 endif
 !
 ! OBS alloc
 !
 if(STRING_match(TRabsWHAT,"k")) then
   YAMBO_ALLOC(Epsilon_ii,(RT_conf%W_n_steps,3)) ! Eps0 Eps Kerr
 else
   YAMBO_ALLOC(Epsilon_ii,(RT_conf%W_n_steps,2)) ! Eps0 Eps
 endif
 !
 ! Frequency setup
 !
 W_bss%er=RT_conf%X_W_range
 W_bss%n_freqs=RT_conf%W_n_steps
 W_bss%per_memstps=100._SP
 W_bss%dr=RT_conf%damp_factor
 !
 call FREQUENCIES_setup(W_bss)
 !
 ! Output
 !
 !call RT_output("open trans_eps_real trans_eps_imag eps_eq trans_residual")
 !if (STRING_match(TRabsWHAT,"r")) call RT_output("open trans_refl_rel trans_refl_abs refl_eq")
 !if (STRING_match(TRabsWHAT,"t")) call RT_output("open trans_transm_rel trans_transm_abs transm_eq")
 !
 call live_timing('Transient Absorption',RT_conf%T_n_steps)
 !
 do i_T=1,RT_conf%T_n_steps
   !
   call RT_apply(DIPs%ib,Xen,Xk,TIME=RT_time(i_T),keep_RT_carriers=.TRUE.)
   !
   Epsilon_ii=cZERO
   N_trans_done=0
   !
   if (STRING_match(TRabsMODE,"eq")) then
     if (BS_H_dim>0 ) call RT_TRabs_residuals("BSE ABS",N_trans_done,DIP_kerr_dir,DIPs%q0,Xen,Xk)
     if (BS_H_dim==0) call RT_TRabs_residuals("IP ABS",N_trans_done,DIP_kerr_dir,DIPs%q0,Xen,Xk)
   endif
   if (STRING_match(TRabsMODE,"vv")) then
     call RT_TRabs_residuals("IP ABS vv",N_trans_done,DIP_kerr_dir,DIPs%q0,Xen,Xk)
   endif
   if (STRING_match(TRabsMODE,"cc")) then
     call RT_TRabs_residuals("IP ABS cc",N_trans_done,DIP_kerr_dir,DIPs%q0,Xen,Xk)
   endif
   !
!   Epsilon_ii(:,2)=Epsilon_ii(:,2)-IP_R*conjg(IP_R)/(W_bss%p(:)-IP_E)
!   Epsilon_ii(:,2)=Epsilon_ii(:,2)+IP_R*conjg(IP_R)/(W_bss%p(:)+IP_E)
   !
!   Epsilon_ii(:,1)=1._SP+Epsilon_ii(:,1)*Co
!   Epsilon_ii(:,2)=      Epsilon_ii(:,2)*Co
!     Epsilon_ii(:,1)=Epsilon_ii(:,1)-BS_R0(i_l)*conjg(BS_R0(i_l))/(W_bss%p(:)-BS_E(i_l))
!     Epsilon_ii(:,2)=Epsilon_ii(:,2)-(BS_R_var +conjg(BS_R_var) )/(W_bss%p(:)-BS_E(i_l))
!     !
!     Epsilon_ii(:,1)=Epsilon_ii(:,1)-BS_R0(i_l)*conjg(BS_R0(i_l))/(W_bss%p(:)+BS_E(i_l))
!     Epsilon_ii(:,2)=Epsilon_ii(:,2)-(BS_R_var +conjg(BS_R_var) )/(W_bss%p(:)+BS_E(i_l))
!     !
!     BS_R_tmp=-real(BS_R_var+conjg(BS_R_var),SP)
     !
!     if( RT_conf%X_W_range(1)<real(BS_E(i_l)) .and. real(BS_E(i_l))<RT_conf%X_W_range(2) ) &
!&       call RT_output("trans_residual",VALUEs=(/BS_R_tmp/),E=real(BS_E(i_l))*HA2EV,TIME=RT_time(i_T)*AUT2FS)
   !
   call live_timing(steps=1)
   !
 enddo

!     !
!     BS_R_tmp=-real(RES_total+conjg(RES_total),SP)
!   if( RT_conf%X_W_range(1)<real(BS_E(i_l)) .and. real(BS_E(i_l))<RT_conf%X_W_range(2) ) then
!     call RT_output("trans_residual",VALUEs=(/BS_R_tmp/),E=real(BS_E(i_l))*HA2EV,TIME=RT_time(i_T)*AUT2FS)
!   endif
 !
 YAMBO_FREE(Epsilon_ii)
 !
 call live_timing()
 !
end subroutine RT_transient_absorption
